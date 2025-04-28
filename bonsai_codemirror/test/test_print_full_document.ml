open! Core
open! Bonsai_web_test
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open Jsdom
module Handle = Handle_experimental

let setup_mock_codemirror_methods () =
  let open Js_of_ocaml in
  let f =
    Js.Unsafe.js_expr
      {js|
    (() => {

function getBoundingClientRect() {
    const rec = {
        x: 0,
        y: 0,
        bottom: 100000,
        height: 100,
        left: 0,
        right: 100,
        top: 0,
        width: 100,
    };
    return {...rec, toJSON: () => rec};
}

class FakeDOMRectList extends Array {
    item(index) {
        return this[index];
    }
}

document.elementFromPoint = () => null;
globalThis.window.HTMLElement.prototype.getBoundingClientRect = getBoundingClientRect;
globalThis.window.HTMLElement.prototype.getClientRects = () => new FakeDOMRectList();
globalThis.window.Range.prototype.getBoundingClientRect = getBoundingClientRect;
globalThis.window.Range.prototype.getClientRects = () => new FakeDOMRectList();

    })
    |js}
  in
  (Js.Unsafe.fun_call f [||] : unit)
;;

module Expect_test_config = struct
  include Expect_test_config

  let run f =
    run (fun () ->
      setup_mock_codemirror_methods ();
      f ())
  ;;
end

let trigger_codemirror_measure () =
  let open Js_of_ocaml in
  let f =
    Js.Unsafe.js_expr
      {js|
        (function() {
          document.querySelector('.cm-content').cmView.view.measure()
        })
      |js}
  in
  (Js.Unsafe.fun_call f [||] : unit)
;;

let%expect_test "demonstrate that print_full_document works" =
  let num_lines = 500 in
  let%bind.With handle =
    Handle.with_ ~get_vdom:fst (fun graph ->
      let print_full_document, set_print_full_document = Bonsai.state false graph in
      let%arr print_full_document and set_print_full_document in
      let view =
        Bonsai_web_ui_codemirror_read_only.make
          ~print_full_document
          ~language:Plaintext
          ~theme:Basic_light
          (String.concat_lines (List.init num_lines ~f:Int.to_string))
      in
      view, set_print_full_document)
  in
  let print_visible_lines () =
    Handle.one_frame handle;
    (* Browser will do it automatically, but we need to trigger it manually in jsdom tests. *)
    trigger_codemirror_measure ();
    let selector = ".cm-line" in
    let nodes =
      Dom_html.document##querySelectorAll (Js.string selector)
      |> Js_of_ocaml__Dom.list_of_nodeList
    in
    print_endline [%string {|Text length: %{num_lines#Int}|}];
    print_endline
      [%string {|Elements matching "%{selector}": %{(List.length nodes)#Int}|}]
  in
  print_visible_lines ();
  [%expect
    {|
    Text length: 500
    Elements matching ".cm-line": 13
    |}];
  Handle.inject handle (fun (_, set_print_full_document) -> set_print_full_document true);
  print_visible_lines ();
  [%expect
    {|
    Text length: 500
    Elements matching ".cm-line": 500
    |}];
  Handle.inject handle (fun (_, set_print_full_document) -> set_print_full_document false);
  print_visible_lines ();
  [%expect
    {|
    Text length: 500
    Elements matching ".cm-line": 13
    |}]
;;
