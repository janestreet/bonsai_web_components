open! Core
open Bonsai_web
module Form := Bonsai_web_ui_form.With_manual_view

(* Bonsai form input elements that use Codemirror as their input elements. Each module
   below provides different behaviour corresponding to the different options in
   [Bonsai_web_ui_codemirror]. *)

(* [Basic] forms accept a static list of extensions. *)
module Basic : sig
  val string
    :  ?name:string
    -> ?extensions:Codemirror.State.Extension.t list
    -> unit
    -> local_ Bonsai.graph
    -> (string, Vdom.Node.t) Form.t Bonsai.t

  val stringable
    :  ?name:string
    -> ?extensions:Codemirror.State.Extension.t list
    -> (module Stringable with type t = 'a)
    -> local_ Bonsai.graph
    -> ('a, Vdom.Node.t) Form.t Bonsai.t

  (** Consider using [Codemirror_sexp.extension] to enable sexp-specific editing features. *)
  val sexpable
    :  ?name:string
    -> ?extensions:Codemirror.State.Extension.t list
    -> (module Sexpable with type t = 'a)
    -> local_ Bonsai.graph
    -> ('a, Vdom.Node.t) Form.t Bonsai.t
end

(* [Dynamic_extensions] forms have the ability to dynamically choose which extensions to
   load, depending on the value of the ['model Bonsai.t] that is supplied. *)
module Dynamic_extensions : sig
  val string
    :  ?sexp_of_model:('model -> Sexp.t)
    -> ?name:string
    -> equal:('model -> 'model -> bool)
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> local_ Bonsai.graph
    -> (string, Vdom.Node.t) Form.t Bonsai.t

  val stringable
    :  (module Stringable with type t = 'a)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?name:string
    -> equal:('model -> 'model -> bool)
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> local_ Bonsai.graph
    -> ('a, Vdom.Node.t) Form.t Bonsai.t

  (** Consider using [Codemirror_sexp.extension] to enable sexp-specific editing features,
      and [Codemirror_sexp.grammar_equal] for [equal]. *)
  val sexpable
    :  (module Sexpable with type t = 'a)
    -> ?sexp_of_model:('model -> Sexp.t)
    -> ?name:string
    -> equal:('model -> 'model -> bool)
    -> compute_extensions:('model -> Codemirror.State.Extension.t list) Bonsai.t
    -> 'model Bonsai.t
    -> local_ Bonsai.graph
    -> ('a, Vdom.Node.t) Form.t Bonsai.t
end
