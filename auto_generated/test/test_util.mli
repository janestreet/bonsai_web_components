open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

val sexp_form_handle
  :  ?optimize:bool
  -> ?get_vdom:('a Form.t -> Vdom.Node.t)
  -> ?customizations:Auto_generated.form_transformer Auto_generated.Customization.t list
  -> ?allow_duplication_of_list_items:bool
  -> (module S with type t = 'a)
  -> ('a Form.t, 'a) Handle.t
