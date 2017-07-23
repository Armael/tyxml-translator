open Tyxml_js
open Tyxml_js.Html5
open Lwt

let lang = Ppx_common.Html

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

    Format.fprintf fmt
      "%a@.%!"
      Mypprintast.expression expr;
    Buffer.contents b

  with Location.Error e ->
    e.Location.msg

let output_text, set_output_text = React.S.create ""
let output_data = R.Html5.pcdata output_text

let optget o = Js.Opt.get o (fun _ -> failwith "oops")

let () =
  let input_box =
    Dom_html.getElementById "input"
    |> Dom_html.CoerceTo.textarea |> optget
  in
  let output_elt = Dom_html.getElementById "output" in
  Lwt.async (fun _ ->
    Lwt_js_events.domContentLoaded () >>= fun _ ->
    output_elt##appendChild (To_dom.of_node output_data) |> ignore;
    return ()
  );

  Lwt.async (fun _ ->
    Lwt_js_events.limited_loop ~elapsed_time:0.5
      Lwt_js_events.input input_box (fun _ _ ->
        let input_text = Js.to_string input_box##.value in
        set_output_text (process input_text);
        return ()
      )
  )
