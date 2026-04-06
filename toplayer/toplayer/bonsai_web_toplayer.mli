open! Core
open! Bonsai_web

module Position : sig
  type t =
    | Auto
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Alignment : sig
  type t =
    | Center
    | Start
    | End
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Offset : sig
  (** Allows controlling how far the floating element is positioned away from the anchor.
      Usually, you don't want to set cross_axis. *)
  type t =
    { main_axis : float
    ; cross_axis : float
    }
  [@@deriving sexp, sexp_grammar, equal, compare]

  (** Apply no offset. *)
  val zero : t
end

module Restore_focus_on_close : sig
  type t =
    | No
    | Yes of { prevent_scroll : bool }
  [@@deriving sexp_of, compare, equal]
end

(** A utility for creating tooltip/popover arrows. You probably want the same colors for
    the arrow and the tooltip / popover. [attrs] should not include padding or size, since
    this is calculated from [arrow_len]. It also should not include positioning, because
    that's set by toplayer. *)
val arrow_helper
  :  attrs:Vdom.Attr.t list
  -> arrow_len:[ `Px_float of float ]
  -> unit
  -> Vdom.Node.t

(** Tooltips can be used to provide additional information to a user when they hover over
    an element. *)
val tooltip
  :  ?tooltip_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?show_delay:Time_ns.Span.t
  -> ?hide_grace_period:Time_ns.Span.t
  -> ?hoverable_inside:bool
  -> ?light_dismiss:bool
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

module Match_anchor_side : sig
  (** [Grow_to_match] will set [min-width] or [min-height]; [Match_exactly] will set
      [width] or [height], and [Shrink_to_match] will set [max-width] or [max-height].

      If not set here, max height and width will be set to the available space. *)
  type t =
    | Grow_to_match
    | Match_exactly
    | Shrink_to_match
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

(** [vdom_popover] returns an attr which, when attached to a Vdom node, will create a
    popover positioned relative to that node.

    The popover's visibility MUST be controlled by attaching / detaching the attr to/from
    the anchor; the browser-level `showPopover()` and `hidePopover()` functions MUST NOT
    be used.

    Prefer using the Bonsai stateful [Popover.*] functions when possible, since that
    correctly handles closing on clicks outside, emitting lifecycle events on open/close,
    etc. *)
val vdom_popover
  :  ?popover_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> ?restore_focus_on_close:Restore_focus_on_close.t
  -> ?overflow_auto_wrapper:bool
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

module Anchor : sig
  type t [@@deriving sexp_of]

  (** [top], [bottom], and [left], [right] are the # of pixels down and right from the top
      left corner to form the (top, bottom), and (left, right) borders of the virtual
      bounding box.

      [relative_to] defaults to [`Viewport], and determines whether the coordinates you
      provide should be in terms of "Client" or "Page" coordinate systems:
      https://developer.mozilla.org/en-US/docs/Web/CSS/CSSOM_view/Coordinate_systems *)
  val of_bounding_box
    :  relative_to:[ `Viewport | `Document ]
    -> top:float
    -> left:float
    -> bottom:float
    -> right:float
    -> t

  (** [x] and [y] are the # of pixels right/down from the top left corner.

      [relative_to] determines whether the coordinates you provide should be in terms of
      viewport/"Client" or document/"Page" coordinate systems:
      https://developer.mozilla.org/en-US/docs/Web/CSS/CSSOM_view/Coordinate_systems *)
  val of_coordinate : relative_to:[ `Viewport | `Document ] -> x:float -> y:float -> t
end

module Close_on_click_outside : sig
  open Js_of_ocaml

  type t =
    | Yes
    | Yes_unless_target_is_popover
    | No
    | Custom of (target:Dom_html.element Js.t Js.opt -> [ `Close | `Don't_close ])

  (** Returns true if the [target] element is inside a popover. (i.e. has a popover as a
      parent at some point) *)
  val is_target_inside_a_popover : target:Dom_html.element Js.t Js.opt -> bool
end

module Controls : sig
  type t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }
end

module Autoclose : sig
  type t = private Vdom.Attr.t

  (** [create] allows you to react to outside clicks and escapes for popovers and modals
      where you own the state.

      Notably, the [close] effect is [bonk]ed, so that if the user clicks on an element
      that opens the popover on click, the [close] will win. This might not be desirable
      when clicking outside to open a _different_ popover, in which case you might want to
      [bonk] your [open_] effect. *)
  val create
    :  close:unit Effect.t Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> local_ Bonsai.graph
    -> t Bonsai.t
end

module Popover : sig
  (** Popovers are a more powerful version of tooltips:

      - [content] has access to a [local_ graph], so they can have internal state.
      - [content] is only active while shown, so it can use [on_activate] and
        [on_deactivate] lifecycle hooks.
      - Multiple popovers can be open at a time; if 2 overlap, they will be stacked in the
        order that they were opened.
      - Bonsai owns popover open/closed state; tooltip state is owned by the browser. *)

  (** If [close_on_click_outside] (default [Yes]), clicks outside of a popover or its DOM
      descendants will close the popover and any descendant popovers.

      [close_on_right_click_outside] (default [No]) behaves similarly. If your popover is
      acting as a menu, you probably want to set this to [Yes].

      If [close_on_esc] (default true), pressing escape will close the focused popover and
      its descendants, and the event will not propagate. If focus is not on any popover,
      all popovers with [close_on_esc] will close.

      If [focus_on_open] (default false) is set to true, the popover root will be focused
      whenever it opens, unless some element was focused [on_activate], or has the HTML
      [autofocus] attribute.

      [position] defaults to [Auto] [alignment] defaults to [Center]

      If [match_anchor_side_length] is set to true, the popover's main axis (width if
      position is Top/Bottom, height if position is Left/Right) will be set to have a
      length equal to the corresponding axis of the anchor. This is particularly useful
      for dropdowns and typeaheads.

      If [overflow_auto_wrapper] (default: [false]), the popover's contents will be
      wrapped in a div with [overflow: auto].

      If you want to run some [unit Effect.t] on close, you can make an [on_deactivate]
      lifecycle hook inside of [content].

      Returns an "anchoring" [Vdom.Attr.t], as well as controls for opening/closing the
      popover. *)
  val create
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?arrow:Vdom.Node.t option Bonsai.t
    -> content:
         (close:unit Effect.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Vdom.Attr.t Bonsai.t * Controls.t

  (** [create_css] doesn't use auto-positioning: these popovers will appear in the top
      left corner of the viewport by default. Callers typically position these via [top]
      and [left] CSS rules.

      [attrs] are required to force you to provide some positioning via css. *)
  val create_css
    :  attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> content:
         (close:unit Effect.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Controls.t

  (** Like [create], but uses a virtual anchor (e.g. a bounding box or a coordinate pair)
      for anchoring instead of a real vdom element. *)
  val create_virtual
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?arrow:Vdom.Node.t option Bonsai.t
    -> content:
         (close:unit Effect.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Anchor.t Bonsai.t
    -> local_ Bonsai.graph
    -> Controls.t

  (** Like [Popover.create], but always open when active. Use by computing / storing your
      own open state, and [match%sub]ing on it. For example:

      {[
        let popover_attr =
          match%sub is_open with
            | Some input ->
              Popover.always_open
                ~content:(fun graph -> ...)
                graph

            | None -> Vdom.Attr.empty
        in
        ...
      ]}

      Typically, you'll calculate [is_open] as a function of some other state you own.

      They will be stacked in the order opened, so the popover whose [is_open] input last
      became [true] will appear on top. *)
  val always_open
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?arrow:Vdom.Node.t option Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Vdom.Attr.t Bonsai.t

  (** Like [always_open], but for [create_css].

      [attrs] are required to force you to provide some positioning via css. *)
  val always_open_css
    :  attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> unit

  (** Like [always_open], but for [create_virtual]. *)
  val always_open_virtual
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?arrow:Vdom.Node.t option Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Anchor.t Bonsai.t
    -> local_ Bonsai.graph
    -> unit
end

module Modal : sig
  (** Modals are like popovers, but when one is open, everything under the modal is inert:
      https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/inert

      If multiple modals are open at the same time, only the topmost one will not be
      inert.

      Inert elements will not be closed when clicking outside them, or on escape.

      Unlike popovers, [close_on_click_outside] defaults to
      [Yes_unless_target_is_popover], because if a popover appears outside of a modal, it
      should likely be interactible without closing the modal.

      Also unlike popovers, [focus_on_open] defaults to [true].

      If [lock_body_scroll] is set to true (default false), scrolling the page behind the
      modal will not be possible.

      If [overflow_auto_wrapper] (default: [false]), the popover's contents will be
      wrapped in a div with [overflow: auto].

      You can style the modal backdrop by targetting the [::backdrop] pseudo-element via
      [attrs]. *)
  val create
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?lock_body_scroll:bool Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?restore_focus_on_close:Restore_focus_on_close.t Bonsai.t
    -> content:
         (close:unit Effect.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Controls.t

  (** Like [Modal.create], but always open when active. Use by computing / storing your
      own open state, and [match%sub]ing on it. For example:

      {[
        let (_ : unit Bonsai.t) =
          match%sub is_open with
            | Some input ->
              Modal.always_open
                ~content:(fun graph -> ...)
                graph;
              return ()
            | None -> return ()
        in
        ...
      ]}

      Typically, you'll calculate [is_open] as a function of some other state you own.

      They will be stacked in the order opened, so the popover whose [is_open] input last
      became [true] will appear on top. *)
  val always_open
    :  ?attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?lock_body_scroll:bool Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?restore_focus_on_close:Restore_focus_on_close.t Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> unit
end

module Private_for_bonsai_web_ui_toplayer : sig
  val create_controls
    :  ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> local_ Bonsai.graph
    -> Autoclose.t Bonsai.t * Controls.t
end
