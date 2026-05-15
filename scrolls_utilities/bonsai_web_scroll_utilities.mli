open! Core
open! Bonsai_web

module How_to_scroll : sig
  (** Used to determine where the chosen pixel would end up inside the scrolling container
      after the scrolling is finished. *)
  type t =
    [ `Minimal
    | `To_bottom
    | `To_top
    | `To_center
    ]
  [@@deriving compare, enumerate, equal, sexp_of]
end

(** Scrolls to a position inside an element found by the provided selector. The target
    element must be relatively or absolutely positioned.

    Returns a [unit Or_error.t Effect.t] that will be an error if the element picked by
    "selector" can't be found.

    This API is *browser-only* and therefore cannot be used in expect tests. *)
val to_position_inside_element
  :  ?smooth:bool
  -> selector:string
  -> x_px:float
  -> y_px:float
  -> How_to_scroll.t
  -> unit Or_error.t Effect.t

(** Similar to the above function, but instead of scrolling to a position, it scrolls to a
    specific element

    This API is *browser-only* and therefore cannot be used in expect tests. *)
val into_view
  :  ?smooth:bool
  -> selector:string
  -> How_to_scroll.t
  -> unit Or_error.t Effect.t
