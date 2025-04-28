open! Core
open! Bonsai_web
open Bonsai_web_ui_toplayer
open Jsdom
module Handle = Handle_experimental
open Util

let filter_printed_attributes = Default_vdom_spec.Expert.filter_printed_attributes ()

let%expect_test "Floating element root is present initially if a graph-consuming \
                 floating element is used."
  =
  let f computation =
    let%bind.With handle =
      Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id computation
    in
    (* The floating root is present, even though no floating elements are currently open. *)
    Handle.print_dom handle;
    assert_not_in_dom handle;
    [%expect
      {|
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;">
            <button> Open </button>
          </div>
        </body>
        <div> </div>
      </html>
      |}]
  in
  f (fun graph ->
    let open Bonsai.Let_syntax in
    let%tydi { open_; _ } =
      Popover.create_css
        ~extra_attrs:(return [])
        ~content:(fun ~close:_ _ -> return {%html|n/a|})
        graph
    in
    let%arr open_ in
    {%html|
      <div id="container">
        <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
      </div>
    |});
  f (fun graph ->
    let open Bonsai.Let_syntax in
    let%tydi attr, { open_; _ } =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ -> return {%html|n/a|})
        graph
    in
    let%arr attr and open_ in
    {%html|
      <div id="container">
        <button id="anchor" %{attr} on_click=%{fun _ -> open_}>Open</button>
      </div>
    |});
  f (fun graph ->
    let open Bonsai.Let_syntax in
    let%tydi { open_; _ } =
      Modal.create
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ -> return {%html|n/a|})
        graph
    in
    let%arr open_ in
    {%html|
      <div id="container">
        <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
      </div>
    |})
;;

let%expect_test "Floating element root is not present initially if only tooltips are \
                 used."
  =
  let%bind.With handle =
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun _ ->
      Bonsai.return {%html|<div %{Tooltip.text "hello"}></div>|})
  in
  (* The floating root is NOT present. *)
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> </div>
      </body>
    </html>
    |}]
;;
