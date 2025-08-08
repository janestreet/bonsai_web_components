open! Core
open! Bonsai_web
open! Bonsai_web_test
module Form = Bonsai_web_ui_form.With_manual_view

val form_result_spec
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ?lint_expected_failures:Linter.Rule.t list
  -> ('a -> Sexp.t)
  -> (('a, Vdom.Node.t) Form.t, 'a) Result_spec.t

val list_form_result_spec
  :  ?filter_printed_attributes:(key:string -> data:string -> bool)
  -> ?censor_paths:bool
  -> ('a -> Sexp.t)
  -> (module Result_spec.S
        with type incoming = [ `Append | `Remove of int | `Set of 'a list ]
         and type t = ('a list, ('a, Vdom.Node.t) Form.Elements.Multiple.t) Form.t)
