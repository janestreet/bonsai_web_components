open! Core
open! Codemirror

let update_diagnostics view prev_errors_ref timeout_ref =
  let editor_state = View.Editor_view.state view in
  let diagnostics = Shared.Parse_driver.errors_unsorted editor_state in
  let errors_same =
    phys_equal diagnostics !prev_errors_ref
    || List.equal phys_equal diagnostics !prev_errors_ref
  in
  match errors_same with
  | true -> ()
  | false ->
    prev_errors_ref := diagnostics;
    Option.iter !timeout_ref ~f:Js_of_ocaml.Dom_html.clearTimeout;
    let timeout =
      Js_of_ocaml.Dom_html.setTimeout
        (fun () ->
          View.Editor_view.dispatch_specs
            view
            (Lint.set_diagnostics ~state:editor_state ~diagnostics);
          timeout_ref := None)
        0.
    in
    timeout_ref := Some timeout
;;

let extension (grammar : 'x Sexp_grammar.t) =
  State.Extension.of_list
    [ Shared.Parse_driver.grammared grammar.untyped
    ; View.View_plugin.extension
        (View.View_plugin.define
           ~create:(fun view ->
             let prev_errors = ref [] in
             let set_errors_timeout = ref None in
             (* If this extension is reconfigured, but the doc isn't changed, we want to
                still show the displayed diagnostics. *)
             let update_diagnostics_on_create =
               Js_of_ocaml.Dom_html.setTimeout
                 (fun () -> update_diagnostics view prev_errors set_errors_timeout)
                 0.
             in
             ignore (update_diagnostics_on_create : Js_of_ocaml.Dom_html.timeout_id_safe);
             { update =
                 Some
                   (fun update ->
                     match Shared.parsed_more update with
                     | false -> ()
                     | true -> update_diagnostics view prev_errors set_errors_timeout)
             ; custom_state = ()
             })
           ())
    ]
;;
