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
    in
    let b = Buffer.create 37 in
    let fmt = Format.formatter_of_buffer b in
    Format.pp_set_margin fmt 78;

    Format.fprintf fmt
      "%a@.%!"
      Pprintast.expression expr;
    Buffer.contents b

  with Location.Error e ->
    e.Location.msg

let output_text, set_output_text = React.S.create []

let output_pre =
  R.Html5.pre (
    React.S.map (List.map pcdata) output_text
    |> ReactiveData.RList.from_signal
  )

let optget o = Js.Opt.get o (fun _ -> failwith "oops")

let () =
  let input_box =
    Dom_html.getElementById "input"
    |> Dom_html.CoerceTo.textarea |> optget
  in
  let output_div = Dom_html.getElementById "output" in
  let btn =
    Dom_html.getElementById "button_go"
    |> Dom_html.CoerceTo.button |> optget
  in
  Lwt.async (fun _ ->
    Lwt_js_events.domContentLoaded () >>= fun _ ->
    output_div##appendChild (To_dom.of_node output_pre) |> ignore;
    return ()
  );

  Lwt.async (fun _ ->
    Lwt_js_events.seq_loop
      Lwt_js_events.click btn (fun _ _ ->
        let input_text = Js.to_string input_box##.value in
        set_output_text [process input_text];
        return ()
      )
  )
