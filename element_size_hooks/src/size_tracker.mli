open! Core
open! Bonsai_web
open! Js_of_ocaml

module Dimension : sig
  type t =
    { width : float
    ; height : float
    }
  [@@deriving compare, equal, sexp_of]

  val zero : t
end

module Dimensions : sig
  (** The "border box" size of an element includes the element itself, borders, padding,
      and any internal scrollbars.

      The "content box" size does not include border or padding.

      When in doubt, default to using [border_box]. It's recommended to set `box-sizing:
      border-box` on all your DOM.

      https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing *)
  type t =
    { border_box : Dimension.t
    ; content_box : Dimension.t
    }
  [@@deriving compare, equal, sexp_of]

  val zero : t
end

(** When attached to a Vdom node, this attribute will monitor the size of this node, and
    report any changes to the size through the provided callback.

    Only changes to the content box will be observed, but both the content and border box
    sizes will be reported to the callback.

    If the callback changes, and is no longer [phys_equal] to itself, it will be called
    with the current size.

    Note: in almost all cases, you'll want to have box-sizing: border-box set on your node
    in order for it to measure the size of the element by looking at the border-size. *)
val on_change : (Dimensions.t -> unit Ui_effect.t) -> Vdom.Attr.t

module For_testing : sig
  val type_id : (Dimensions.t -> unit Vdom.Effect.t) Type_equal.Id.t
end
