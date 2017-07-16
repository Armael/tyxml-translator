open Asttypes
open Parsetree
open Ast_helper
open Ast_mapper

(* Let Tyxml's PPX do all the hard work: parse the string [s] as [lang] (Html or
   Svg), and return a corresponding OCaml expression that uses Tyxml
   combinators.
*)
let ocaml_of_html (lang : Ppx_common.lang) (s : string) : expression =
  Ppx_tyxml.markup_to_expr
    lang Location.none
    [Exp.constant (Const.string s)]


(* At this point, we could use [Pprintast.expression] to pretty-print the
   expression and be done. However, due to the generic architecture of the PPX,
   the generated OCaml code is not fully specialized, and uses a "wrapping"
   module of type [Xml_wrap.T], and typically named [lang.Xml.W].

   What follows is some machinery to inline the wrapping functions for common
   instances of the wrapping module (at the moment, only "identity wrapping", in
   the future, possibly "reactive nodes wrapping"...).
*)

(* Defines how to rewrite the wrapping functions.

   This assumes the wrap functions are always fully applied... At the moment,
   the PPX satisfies this assumption.
*)
type rewrite_wrap = {
  return : expression -> expression;
  fmap : expression -> expression -> expression;

  nil : expression -> expression;
  singleton : expression -> expression;
  cons : expression -> expression -> expression;
  append : expression -> expression -> expression;
  map : expression -> expression -> expression;
}

(* Rewriting rules for wrappers that correspond to the [Xml_wrap.NoWrap] module,
   i.e. the wrapping functions do not do anything particular.
*)
let nowrap : rewrite_wrap =
  let mkcons e1 e2 =
    Exp.construct
      (Location.mknoloc (Longident.Lident "::"))
      (Some (Exp.tuple [e1; e2]))
  in
  {
    return = (fun x -> x);
    fmap = (fun f arg -> Exp.apply f [Nolabel, arg]);
    nil = (fun _ -> Ppx_common.list Location.none []);
    singleton = (fun e -> Ppx_common.list Location.none [e]);
    cons = mkcons;
    append = (fun e1 e2 ->
      Exp.apply (Exp.ident (Location.mknoloc (Longident.Lident "@")))
        [(Nolabel, e1); (Nolabel, e2)]
    );
    map = (fun e1 e2 ->
      Exp.apply
        (Exp.ident (Location.mknoloc (Longident.(Ldot (Lident "List", "map")))))
        [(Nolabel, e1); (Nolabel, e2)]
    );
  }

(* Check if a [Longident.t] belongs to the wrapping module, for the given
   [lang].
*)
let lang_mod : Ppx_common.lang -> string = function
  | Ppx_common.Html -> "Html"
  | Ppx_common.Svg -> "Svg"

let is_wrap (lang : Ppx_common.lang) (lid : Longident.t) : string option =
  let open Longident in
  match Longident.flatten lid with
  | [modn; "Xml"; "W"; s] when modn = lang_mod lang -> Some s
  | _ -> None

(* A parsetree mapper that unfolds wrapping functions, for [lang], according to
   the rewrite functions of [rw].
*)
let unfold_wrap_mapper (lang : Ppx_common.lang) (rw : rewrite_wrap) =
  let isw = is_wrap lang in
  let rec expr mapper e =
    match e with
    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident { txt = lid } },
            args
          ) } ->

      let (!!) = expr mapper in
      begin match isw lid, List.map snd args with
        | Some "return", [e] -> rw.return !!e
        | Some "fmap", [e1; e2] -> rw.fmap !!e1 !!e2
        | Some "nil", [e] -> rw.nil !!e
        | Some "singleton", [e] -> rw.singleton !!e
        | Some "cons", [e1; e2] -> rw.cons !!e1 !!e2
        | Some "append", [e1; e2] -> rw.append !!e1 !!e2
        | Some "map", [e1; e2] -> rw.map !!e1 !!e2
        | Some _, _ -> failwith "Unhandled or partially applied wrapping function"
        | None, _ -> default_mapper.expr mapper e
      end

    | other -> default_mapper.expr mapper other
  in

  { default_mapper with expr }

let unfold_wrap
    (lang : Ppx_common.lang)
    (rw : rewrite_wrap) :
  expression -> expression
  =
  let mapper = unfold_wrap_mapper lang rw in
  mapper.expr mapper

(******************************************************************************)

(* The PPX also currently qualifies combinators names with their modules.
   E.g. the function for a "h1" Html element will be [Html.h1].

   This is very reasonable in the setting of a PPX; here we prefer to have a
   topmost [let open Html in]. To that effect, the following parsetree mapper
   removes the module qualifier from the combinator names.
*)
let unqualify_mapper (lang : Ppx_common.lang) =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_ident Location.{ txt = lid; loc } } ->
        let open Longident in
        let lid' = match lid with
          | Ldot (Lident modname, fname) when
              modname = lang_mod lang -> Lident fname
          | _ -> lid
        in
        { expr with
          pexp_desc =
            Pexp_ident Location.{ txt = lid'; loc } }
      | other -> default_mapper.expr mapper other }

let unqualify (lang : Ppx_common.lang) : expression -> expression =
  let mapper = unqualify_mapper lang in
  mapper.expr mapper

(* (\* Turn [e] into [let open ... in e] *\) *)
(* let add_local_open (lang : Ppx_common.lang) (e : expression) : expression = *)
(*   Exp.open_ Fresh (Location.mknoloc (Longident.Lident (lang_mod lang))) e *)

(******************************************************************************)

(* Best-effort cleanup of non-significant whitespace.

   Motivation: when the user inputs well-indented HTML, the resulting HTML and
   OCaml ASTs contain a bunch of whitespace-only text nodes. These text nodes
   clutter the output, and in most cases, they will not be significant at
   rendering time.

   This gives the motivation for some rewriting pass which cleans up whitespace
   "with no semantic meaning".

   However, rules for deciding of the semantics of white-space (at rendering
   time) are quite intricate (see e.g.
   https://medium.com/@patrickbrosset/when-does-white-space-matter-in-html-b90e8a7cdd33
   for an introduction) — and are ultimately determined not only by the HTML,
   but also by the CSS. For example, CSS can make all whitespace significant,
   using the [white-space] property.

   Therefore, we implement a "best-effort" cleanup pass. "best-effort" means
   that we do not guarantee formal correctness properties for its output. We
   keep the implementation simple, and hope that in 90% of cases it matches the
   user expectations, i.e. produces an output semantically equivalent to the
   input.

   It also means that this pass should be optional.

   The code below implements the following, simple-minded transformation:

   When not in a <pre>:
   - merge consecutive whitespace characters into a single ' ' for all children
     text nodes;
   - if the first child is a text node, trim whitespace at the beginning, or remove
     it if it is only whitespace
   - symmetrically for the last child, trim whitespace at its end, or remove it
     if only whitespace
*)

let char_is_whitespace = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false

let str_is_whitespace (s : string) =
  let rec loop i =
    if i >= String.length s then true
    else char_is_whitespace s.[i] && loop (i+1)
  in
  loop 0

let str_trim_beginning, str_trim_end =
  let rec skip s i finished next =
    if finished i || not (char_is_whitespace s.[i]) then i
    else skip s (next i) finished next
  in
  (fun s ->
     let n = String.length s in
     let j = skip s 0 (fun i -> i >= n) succ in
     String.sub s j (String.length s - j)),
  (fun s ->
     let n = String.length s in
     let j = skip s (n - 1) (fun i -> i < 0) pred in
     String.sub s 0 (j+1))

let collapse_whitespace s =
  let b = Buffer.create (String.length s) in
  let in_whitespace = ref false in
  for i = 0 to String.length s - 1 do
    if char_is_whitespace s.[i] then (
      if !in_whitespace then ()
      else (
        in_whitespace := true;
        Buffer.add_char b ' '
      )
    ) else (
      Buffer.add_char b s.[i];
      in_whitespace := false;
    )
  done;
  Buffer.contents b

let elt_is_pcdata (e : expression) =
  match e with
  | { pexp_desc =
        Pexp_apply (
          { pexp_desc = Pexp_ident { txt = lid } },
          [_, { pexp_desc = Pexp_constant (Pconst_string (s, None)) }]
        ) }
    when Longident.flatten lid = ["pcdata"] -> Some s
  | _ -> None

let elt_is_whitespace e =
  match elt_is_pcdata e with
  | Some s -> str_is_whitespace s
  | None -> false

let pcdata_with ~f ~s e =
  match e with
  | { pexp_desc = Pexp_apply (fe, [lbl, _]) } ->
    { e with pexp_desc = Pexp_apply (f fe, [lbl, Exp.constant (Const.string s)]) }
  | _ -> failwith "pcdata_with"

let pcdata_map ~default ~f e =
  match elt_is_pcdata e with
  | Some s -> pcdata_with ~f:default ~s:(f s) e
  | None -> default e

let split_last l =
  let lr = List.rev l in
  (List.rev (List.tl lr), List.hd lr)

let dest_cons e =
  match e with
  | { pexp_desc =
        Pexp_construct ({ txt = lid },
                        Some e') }
    when Longident.flatten lid = ["::"] ->
    begin match e' with
      | { pexp_desc = Pexp_tuple [e1; e2] } -> Some (e1, e2)
      | _ -> None
    end
  | _ -> None

let is_nil e =
  match e with
  | { pexp_desc = Pexp_construct ({ txt = lid }, None) }
    when Longident.flatten lid = ["[]"] -> true
  | _ -> false

let rec dest_list e =
  if is_nil e then Some []
  else
    match dest_cons e with
    | Some (e1, e2) ->
      begin match dest_list e2 with
        | Some l -> Some (e1 :: l)
        | None -> None
      end
    | None -> None

let cleanup_whitespace_mapper =
  let rec expr mapper e =
    let (!!) = expr mapper in
    match e with
    | { pexp_desc =
          Pexp_apply ({ pexp_desc = Pexp_ident { txt = lid } }, _) }
      when Longident.flatten lid = ["pre"] ->
      e

    | { pexp_desc = Pexp_apply (f, args) } ->
      let (front, (lbln, argn)) = split_last args in
      let front' = List.map (fun (lbl, arg) -> (lbl, !!arg)) front in
      let argn' =
        begin match dest_list argn with
          | Some l ->
            let l = List.map (pcdata_map ~default:(!!) ~f:collapse_whitespace) l in
            begin match l with
              | [] -> argn
              | [e'] ->
                let e'' =
                  pcdata_map
                    ~default:(!!)
                    ~f:(fun s -> str_trim_beginning @@ str_trim_end s)
                    e'
                in
                Ppx_common.list Location.none [e'']
              | e1' :: es' ->
                let es'_middle, en' = split_last es' in
                let ret =
                  (if elt_is_whitespace e1' then []
                   else [pcdata_map ~default:(!!) ~f:str_trim_beginning e1'])
                  @ es'_middle @
                  (if elt_is_whitespace en' then []
                   else [pcdata_map ~default:(!!) ~f:str_trim_end en'])
                in
                Ppx_common.list Location.none ret
            end

          | None -> !!argn
        end
      in
      { e with pexp_desc = Pexp_apply (!!f, front' @ [lbln, argn']) }

    | _ -> default_mapper.expr mapper e
  in
  { default_mapper with expr }

let cleanup_whitespace : expression -> expression =
  cleanup_whitespace_mapper.expr cleanup_whitespace_mapper
