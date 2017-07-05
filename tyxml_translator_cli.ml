(* A simple CLI program that reads html/svg, and outputs corresponding OCaml
   code using Tyxml combinators.
*)

let anon_args = ref []
let lang = ref Ppx_common.Html

let add_anon_arg x =
  anon_args := x :: !anon_args

let () = Arg.parse (Arg.align [
  "--html", Arg.Unit (fun () -> lang := Ppx_common.Html),
    " Set input format to html (the default)";
  "--svg", Arg.Unit (fun () -> lang := Ppx_common.Svg),
    " Set input format to svg";
  "-", Arg.Unit (fun () -> add_anon_arg "-"),
    "";
]) add_anon_arg
(Printf.sprintf
  "Usage: %s [--html | --svg] [INPUT]...

Convert html or svg to the corresponding tyxml combinators.

INPUT is either an input html/svg fragment, or -, to read the input from stdin.
"
  Sys.argv.(0))

let process (s : string) =
  try
    let expr =
      Translator.ocaml_of_html !lang s
      |> Translator.unfold_wrap !lang Translator.nowrap
      |> Translator.unqualify !lang
    in
    Format.fprintf Format.std_formatter
      "let open Tyxml.Expr in@.%a@.%!"
      Pprintast.expression expr;

  with Location.Error e ->
    Format.fprintf Format.err_formatter
      "%s%!" e.Location.msg

(* From containers' CCIO.ml *)
let read_all
: ?size:int -> in_channel -> string
= fun ?(size = 1024) ic ->
  let buf = ref (Bytes.create size) in
  let len = ref 0 in
  try
    while true do
      (* resize *)
      if !len = Bytes.length !buf then (
        buf := Bytes.extend !buf 0 !len;
      );
      assert (Bytes.length !buf > !len);
      let n = input ic !buf !len (Bytes.length !buf - !len) in
      len := !len + n;
      if n = 0 then raise Exit;  (* exhausted *)
    done;
    assert false (* never reached*)
  with Exit ->
    Bytes.sub_string !buf 0 !len

let () =
  List.iter (fun s ->
    let input = if s = "-" then read_all stdin else s in
    process input
  ) (List.rev !anon_args)
