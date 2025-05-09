open! Core
open! Bonsai_web
module Form = Bonsai_web_ui_form.With_manual_view
open Jsdom
module Handle = Handle_experimental

let filter_printed_attributes = Default_vdom_spec.Expert.filter_printed_attributes ()

let print_element_value handle ~selector =
  let open Js_of_ocaml in
  let element = Handle.query_selector_exn handle ~selector in
  let with_value : < value : Js.js_string Js.t Js.optdef_prop > Js.t =
    Js.Unsafe.coerce element
  in
  match with_value##.value |> Js.Optdef.to_option |> Option.map ~f:Js.to_string with
  | None -> print_endline "Could not get value: not an input!"
  | Some value -> print_endline value
;;

let%expect_test "Form.set is reflected within a frame." =
  let%bind.With handle =
    Handle.with_
      ~filter_printed_attributes
      ~get_vdom:(fun { Form.view; _ } -> view)
      (fun (local_ graph) ->
        Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph)
  in
  let inject s = Handle.inject handle (fun { Form.set; _ } -> set s) in
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <input type="text" tabindex="0" style="outline: none;"/>
      </body>
    </html>
    |}];
  inject "hi";
  (* Nothing immediately, because we need to go through the action queue. *)
  Handle.print_dom_diff ~context:1 handle;
  [%expect {| |}];
  print_element_value handle ~selector:"input";
  [%expect {| |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:1 handle;
  [%expect {| |}];
  print_element_value handle ~selector:"input";
  [%expect {| hi |}]
;;
