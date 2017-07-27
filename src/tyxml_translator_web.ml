open Lwt.Infix

let setup_format_tags fmt =
  let handle_tag f tag =
    match tag with
    | "keyword" | "ident" | "modident" | "string" | "char" | "label-arg" ->
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

let process lang cleanup_whitespace (s : string) =
  try
    Translator.ocaml_of_html lang s
    |> Translator.unfold_wrap lang Translator.nowrap
    |> Translator.unqualify lang
    |> (fun x -> if cleanup_whitespace then Translator.cleanup_whitespace x else x)
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
  let input_is_html = Dom_html.(getElementById "input-html" |> CoerceTo.input |> optget) in
  let input_is_svg = Dom_html.(getElementById "input-svg" |> CoerceTo.input |> optget) in
  let input_cleanup_whitespace = Dom_html.(getElementById "output-whitespace" |> CoerceTo.input |> optget) in

  let output_width () =
    let output_panel_width = output_panel##.offsetWidth in
    let one_space_width = one_space_span##.offsetWidth in
    min (output_panel_width / one_space_width - 3) 80
  in

  let selected_lang () =
    if input_is_svg##.checked |> Js.to_bool then Ppx_common.Svg
    else Ppx_common.Html
  in

  let do_cleanup_whitespace () =
    input_cleanup_whitespace##.checked |> Js.to_bool
  in

  let input, set_input = React.S.create "" in
  let resized, trigger_resized = React.E.create () in
  let lang_changed, trigger_lang_changed = React.E.create () in
  let cleanup_whitespace_changed, trigger_cleanup_whitespace_changed = React.E.create () in

  let of_event f init e =
    React.E.map f e |> React.S.hold (f init)
  in

  let width = of_event output_width () resized in
  let lang = of_event selected_lang () lang_changed in
  let cleanup_whitespace = of_event do_cleanup_whitespace () cleanup_whitespace_changed in
  let output = React.S.l3 process lang cleanup_whitespace input in

  let _display_output = React.S.l2 (fun width out ->
    output_elt##.innerHTML := Js.string (pp_result width out)
  ) width output in

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.5
      Lwt_js_events.input input_box
      (fun _ _ -> Lwt.return (set_input (Js.to_string input_box##.value)))
  );

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.1
      (fun ?use_capture -> Lwt_js_events.onresize) ()
      (fun _ _ -> Lwt.return (trigger_resized ()))
  );

  Lwt.async (fun _ ->
    Lwt_js_events.seq_loop
      (fun ?use_capture (t1, t2) -> Lwt_js_events.(click t1 <?> click t2))
      (input_is_html, input_is_svg)
      (fun _ _ -> Lwt.return (trigger_lang_changed ()))
  );

  Lwt.async (fun _ ->
    Lwt_js_events.seq_loop Lwt_js_events.click input_cleanup_whitespace
      (fun _ _ -> Lwt.return (trigger_cleanup_whitespace_changed ()))
  )
