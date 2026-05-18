open! Core
open! Bonsai_web

(** A utility for creating tooltip/popover arrows. You probably want the same colors for
    the arrow and the tooltip / popover. [attrs] should not include padding or size, since
    this is calculated from [arrow_len], or positioning, because that's set by toplayer. *)
val arrow_helper
  :  attrs:Vdom.Attr.t list
  -> arrow_len:[ `Px_float of float ]
  -> unit
  -> Vdom.Node.t

module Tooltip : sig
  type t =
    { tooltip_attrs : Vdom.Attr.t list
    ; anchor_attrs : Vdom.Attr.t list
    ; main_axis_offset : float
    ; cross_axis_offset : float
    ; show_delay : Time_ns.Span.t option
    ; hide_grace_period : Time_ns.Span.t option
    ; hoverable_hide_grace_period : Time_ns.Span.t
    (** [hoverable_hide_grace_period] is required, because some time is needed for the
        cursor to move into the tooltip. *)
    ; arrow : Vdom.Node.t option
    }

  (** [default_anchor_attrs] sets a dotted underline on tooltip anchor text. *)
  val default_anchor_attrs : Vdom.Attr.t

  (** [tooltip_attrs] and [arrow] should have the same background colors and borders. We
      recommend using the [arrow_helper] to create arrows. [anchor_attrs] defaults to
      [default_anchor_attrs] [main_axis_offset] and [cross_axis_offset] default to [0.]
      [show_delay] and [hide_grace_period] default to [None] [hoverable_hide_grace_period]
      defaults to 150ms *)
  val create
    :  tooltip_attrs:Vdom.Attr.t list
    -> arrow:Vdom.Node.t option
    -> ?anchor_attrs:Vdom.Attr.t list
    -> ?main_axis_offset:float
    -> ?cross_axis_offset:float
    -> ?show_delay:Time_ns.Span.t
    -> ?hide_grace_period:Time_ns.Span.t
    -> ?hoverable_hide_grace_period:Time_ns.Span.t
    -> unit
    -> t
end

module Popover : sig
  type t =
    { popover_attrs : Vdom.Attr.t list
    ; default_main_axis_offset : float
    ; default_main_axis_offset_with_arrow : float
    ; arrow : Vdom.Node.t
    }

  (** [popover_attrs] and [arrow] should have the same background colors and borders. We
      recommend using the [arrow] helper function to create arrows.

      [default_main_axis_offset] defaults to [0.] [default_main_axis_offset_with_arrow]
      defaults to [12.] [arrow] defaults to [default_arrow ~arrow_len:(`Px_float 8.) ()] *)
  val create
    :  popover_attrs:Vdom.Attr.t list
    -> arrow:Vdom.Node.t
    -> ?default_main_axis_offset:float
    -> ?default_main_axis_offset_with_arrow:float
    -> unit
    -> t
end

module Modal : sig
  type t = { modal_attrs : Vdom.Attr.t list }
end
