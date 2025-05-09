open! Core
open! Bonsai_web
open! Import

module Side : sig
  type t =
    | First (* Left or top side *)
    | Second
end

module Direction : sig
  type t =
    | Horizontal (* default *)
    | Vertical
end

(** This attribute, when added to a Vdom node adds the ability for that dom-node to be
    clicked-and-dragged to change its parents width. *)
val attr
  :  ?on_drag_start:unit Effect.t
  -> ?on_drag:(float -> unit Effect.t)
       (** Size of the border-box in px. Fired inside a requestAnimationFrame loop during
           the drag interaction. *)
  -> ?on_drag_stop:unit Effect.t
  -> ?freeze_size:[ `Always | `While_dragging ]
       (** Controls how the size of the parent is fixed via [Freeze]. [`Always] will
           freeze the size of the parent on mount to prevent layout shifts.
           [`While_dragging] will freeze the size of the parent only during the drag
           interaction. Defaults to [`Always]. *)
  -> ?direction:Direction.t
  -> side:Side.t
  -> unit
  -> Vdom.Attr.t
