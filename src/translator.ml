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
