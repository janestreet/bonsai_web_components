open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

let sexp_form_handle
  (type a)
  ?optimize
  ?get_vdom
  ?customizations
  ?allow_duplication_of_list_items
  (module M : S with type t = a)
  =
  let form =
    Auto_generated.form
      (module M)
      ?customizations
      ?allow_duplication_of_list_items
      ~allow_updates_when_focused:`Never
  in
  Handle.create ?optimize (form_result_spec ?get_vdom M.sexp_of_t) form
;;
