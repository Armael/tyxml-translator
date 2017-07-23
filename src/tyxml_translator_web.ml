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
    Translator.ocaml_of_html lang s
    |> Translator.unfold_wrap lang Translator.nowrap
    |> Translator.unqualify lang
    |> Translator.cleanup_whitespace
    |> fun s -> `Ok s
  with Location.Error e ->
    `Error e.Location.msg

let pp_result width res =
  let b = Buffer.create 37 in
  let fmt = Format.formatter_of_buffer b in
  Format.pp_set_margin fmt width;
  setup_format_tags fmt;

  begin match res with
  | `Ok expr ->
    Format.fprintf fmt
      "%a@.%!"
      Mypprintast.expression expr
  | `Error s ->
    Format.fprintf fmt
      "%s@.%!" s
  end;
  Buffer.contents b

let optget o = Js.Opt.get o (fun _ -> failwith "oops")

let () =
  let input_box =
    Dom_html.getElementById "input"
    |> Dom_html.CoerceTo.textarea |> optget
  in
  let output_panel = Dom_html.getElementById "output-panel" in
  let output_elt = Dom_html.getElementById "output" in
  let one_space_span = Dom_html.getElementById "one-space-hidden-span" in

  let output_width () =
    let output_panel_width = output_panel##.offsetWidth in
    let one_space_width = one_space_span##.offsetWidth in
    min (output_panel_width / one_space_width - 3) 80
  in

  let current_result = ref (`Error "") in
  let current_width = ref (output_width ()) in

  let display_result () =
    output_elt##.innerHTML :=
      Js.string (pp_result !current_width !current_result)
  in

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.5
      Lwt_js_events.input input_box (fun _ _ ->
        let input_text = Js.to_string input_box##.value in
        current_result := (process input_text);
        display_result ();
        return ()
      )
  );

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.1
      (fun ?use_capture -> Lwt_js_events.onresize) () (fun _ _ ->
        current_width := output_width ();
        display_result ();
        return ()
      )
  )
