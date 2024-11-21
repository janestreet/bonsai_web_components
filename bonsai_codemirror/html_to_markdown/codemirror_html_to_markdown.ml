open! Core
open Js_of_ocaml
open Codemirror

let extension =
  View.Editor_view.dom_event_handlers
    (View.Dom_event_handlers.create
       ~paste:(fun event view ->
         let event : Dom_html.clipboardEvent Js.t = Js.Unsafe.coerce event in
         match Js.to_string (event##.clipboardData##getData (Js.string "text/html")) with
         | "" -> ()
         | html ->
           Dom.preventDefault event;
           print_endline html;
           let soup = Lambda_soup_js.parse html in
           let markdown = Html_to_markdown.convert soup in
           View.Editor_view.dispatch_specs
             view
             (State.Editor_state.replace_selection
                (View.Editor_view.state view)
                (Text.Text.of_ (String.split_lines markdown))))
       ())
;;
