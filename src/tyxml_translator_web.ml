open Lwt

let lang = Ppx_common.Html

let setup_format_tags fmt =
  let handle_tag f tag =
    match tag with
    | "keyword" | "ident" | "modident" | "string" | "char" ->
      f tag
    | _ -> ""
  in
  let ftf = {
    (Format.get_formatter_tag_functions ()) with
    Format.mark_open_tag = handle_tag (fun tag ->
      Printf.sprintf "<span class=\"%s\">" tag
    );
    Format.mark_close_tag = handle_tag (fun _ ->
      "</span>"
    );
  } in

  Format.pp_set_formatter_tag_functions fmt ftf;
  Format.pp_set_tags fmt true

let process (s : string) =
  try
    let expr =
      Translator.ocaml_of_html lang s
      |> Translator.unfold_wrap lang Translator.nowrap
      |> Translator.unqualify lang
      |> Translator.cleanup_whitespace
    in
    let b = Buffer.create 37 in
    let fmt = Format.formatter_of_buffer b in
    Format.pp_set_margin fmt 78;

    setup_format_tags fmt;
    Format.fprintf fmt
      "%a@.%!"
      Mypprintast.expression expr;
    Buffer.contents b

  with Location.Error e ->
    e.Location.msg

let optget o = Js.Opt.get o (fun _ -> failwith "oops")

let () =
  let input_box =
    Dom_html.getElementById "input"
    |> Dom_html.CoerceTo.textarea |> optget
  in
  let output_elt = Dom_html.getElementById "output" in

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.5
      Lwt_js_events.input input_box (fun _ _ ->
        let input_text = Js.to_string input_box##.value in
        output_elt##.innerHTML := Js.string (process input_text);
        return ()
      )
  )
