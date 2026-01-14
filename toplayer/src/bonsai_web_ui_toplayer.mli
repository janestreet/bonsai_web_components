open! Core
open! Bonsai_web
module Styling = Bonsai_web_ui_toplayer_styling

(** NOTE: This library is implemented in terms of [Byo_toplayer]. The implementation in
    [Bonsai_web_ui_toplayer.ml] is mainly a wrapper, providing [Config.t]s, themability,
    etc.

    Toplayer elements will not show up in tests by default, because they are portalled
    outside of the app root. You should use [bonsai_web_ui_toplayer_test] as a helper
    library in your testing.

    [bonsai_web_ui_toplayer] contains pure vdom tooltips, and stateful Bonsai popovers and
    modals: UI elements that appear in the browser top layer, on top of everything else in
    your web UI.

    The [Vdom.Attr.t]s produced by these computations will position the popover/modal/etc
    relative to the DOM node where the attr is attached.

    The main difference between popovers and modals is that modals "block" the page
    content under them. For instance:
    - Moving your mouse outside of a modal should not trigger `hover` on any elements not
      in that modal.
    - Clicking outside of a modal will close only that modal, and click events will not
      propagate.

    All default stylings can be overriden through the [for_toplayer] methods of the
    [View.Theme.t].

    These implementations retain their state when closed, including whether any nested
    elements are open. You might want to wrap their contents in a
    [Bonsai.with_model_resetter'], and use an [on_deactivate] lifecycle hook to clear
    state, or a [Bonsai.scope_model] if the popover/modal is keyed by one of multiple
    inputs.

    Authors of reusable components might also be interested in the [byo_toplayer] library,
    which is the foundation for this one. *)

module Position = Byo_toplayer.Position
module Alignment = Byo_toplayer.Alignment
module Offset = Byo_toplayer.Offset
module Restore_focus_on_close = Byo_toplayer.Restore_focus_on_close

(** A utility for creating tooltip/popover arrows. You probably want the same colors for
    the arrow and the tooltip / popover. [attrs] should not include padding or size, since
    this is calculated from [arrow_len]. It also should not include positioning, because
    that's set by toplayer. *)
val arrow_helper
  :  attrs:Vdom.Attr.t list
  -> arrow_len:[ `Px_float of float ]
  -> unit
  -> Vdom.Node.t

module Tooltip : sig
  module Config : sig
    (** A [Tooltip.Config.t] includes "theme-like" appearance configuration for tooltips;
        that is, things that you'll likely want to share across all tooltips in your app. *)
    type t = Styling.Tooltip.t =
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

    (** You are responsible for providing background + foreground colors and borders as
        part of [tooltip_attrs].

        [tooltip_attrs] and [arrow] should have the same background colors and borders.

        [arrow] defaults to [None].We recommend using the [arrow_helper] to create arrows.
        [anchor_attrs] defaults to [default_anchor_attrs] [main_axis_offset] and
        [cross_axis_offset] default to [0.] [show_delay] and [hide_grace_period] default
        to [None] [hoverable_hide_grace_period] defaults to 150ms *)
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

  (** Tooltips can be used to provide additional information to a user when they hover
      over an element.

      [config] defaults to [Config.default ()], although we recommend using the config
      provided by your component library. [position] defaults to [Auto] [alignment]
      defaults to [Center] [hoverable_inside] defaults to [false]. *)
  val create
    :  ?config:Config.t
    -> ?position:Position.t
    -> ?alignment:Alignment.t
    -> ?hoverable_inside:bool
    -> Vdom.Node.t
    -> Vdom.Attr.t

  (** Like [Tooltip.create], but accepts a string. *)
  val text
    :  ?config:Config.t
    -> ?position:Position.t
    -> ?alignment:Alignment.t
    -> ?hoverable_inside:bool
    -> string
    -> Vdom.Attr.t
end

module Match_anchor_side = Byo_toplayer.Match_anchor_side

module Anchor : sig
  type t = Byo_toplayer.Anchor.t [@@deriving sexp_of]

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

  type t = Byo_toplayer.Close_on_click_outside.t =
    | Yes
    | Yes_unless_target_is_popover
    | No
    | Custom of (target:Dom_html.element Js.t Js.opt -> [ `Close | `Don't_close ])

  (** Returns true if the [target] element is inside a popover. (i.e. has a popover as a
      parent at some point) *)
  val is_target_inside_a_popover : target:Dom_html.element Js.t Js.opt -> bool
end

module Autoclose : sig
  type t = private Vdom.Attr.t

  (** [create] allows you to react to outside clicks and escapes for popovers and modals.
      It's particularly useful when you own the open/closed state. *)
  val create
    :  close:unit Effect.t Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> local_ Bonsai.graph
    -> t Bonsai.t
end

module Controls : sig
  type t = Byo_toplayer.Controls.t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }
end

module Popover : sig
  (** Popovers are a more powerful version of tooltips:

      - [content] has access to a [local_ graph], so they can have internal state.
      - [content] is only active while shown, so it can use [on_activate] and
        [on_deactivate] lifecycle hooks.
      - Multiple popovers can be open at a time; if 2 overlap, they will be stacked in the
        order that they were opened.
      - Bonsai owns popover open/closed state; tooltip state is owned by the browser. *)

  module Config : sig
    (** A [Popover.Config.t] includes "theme-like" appearance configuration for popovers;
        that is, things that you'll likely want to share across all popovers in your app. *)
    type t = Styling.Popover.t =
      { popover_attrs : Vdom.Attr.t list
      ; default_main_axis_offset : float
      ; default_main_axis_offset_with_arrow : float
      ; arrow : Vdom.Node.t
      }

    (** You are responsible for providing background + foreground colors and borders as
        part of [tooltip_attrs].

        [popover_attrs] and [arrow] should have the same background colors and borders. We
        recommend using the [arrow] helper function to create arrows.

        [default_main_axis_offset] defaults to [0.], and will be used if the popover
        doesn't specify an offset. [default_main_axis_offset_with_arrow] defaults to
        [12.], and will be used if the popover doesn't specify an offset, and has an
        arrow. [arrow] defaults to [default_arrow ~arrow_len:(`Px_float 8.) ()] It will be
        used if you create a popover with [~has_arrow:true]. *)
    val create
      :  popover_attrs:Vdom.Attr.t list
      -> arrow:Vdom.Node.t
      -> ?default_main_axis_offset:float
      -> ?default_main_axis_offset_with_arrow:float
      -> unit
      -> t
  end

  (** [config] defaults to [`From_theme], which pulls from the dynamically-scoped
      [View.Theme.t].

      If [close_on_click_outside] (default [Yes]), clicks outside of a popover or its DOM
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
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
    -> content:
         (close:unit Effect.t Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Vdom.Attr.t Bonsai.t * Controls.t

  (** [create_css] doesn't use auto-positioning: these popovers will appear in the top
      left corner of the viewport by default. Callers typically position these via [top]
      and [left] CSS rules.

      [extra_attrs] are required to force you to provide some positioning via css. *)
  val create_css
    :  extra_attrs:Vdom.Attr.t list Bonsai.t
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
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?close_on_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_right_click_outside:Close_on_click_outside.t Bonsai.t
    -> ?close_on_esc:bool Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
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
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> Vdom.Attr.t Bonsai.t

  (** Like [always_open], but for [create_css].

      [attrs] are required to force you to provide some positioning via css. *)
  val always_open_css
    :  extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> unit

  val always_open_virtual
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?position:Position.t Bonsai.t
    -> ?alignment:Alignment.t Bonsai.t
    -> ?offset:Offset.t Bonsai.t
    -> ?match_anchor_side_length:Match_anchor_side.t option Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?has_arrow:bool Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> Anchor.t Bonsai.t
    -> local_ Bonsai.graph
    -> unit
end

module Modal : sig
  module Config : sig
    (** A [Modal.Config.t] includes "theme-like" appearance configuration for popovers;
        that is, things that you'll likely want to share across all popovers in your app.

        Importantly, modals don't have any positioning by default, and will appear in the
        top-left corner of the page, without any backdrop. We recommend that your
        [Styling.Modal.t] include some positioning (e.g. [margin: auto]), and apply a
        backdrop. *)
    type t = Styling.Modal.t = { modal_attrs : Vdom.Attr.t list }
  end

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
      [extra_attrs].

      [config] defaults to [`From_theme]. *)

  val create
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
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
    :  ?config:[ `This_one of Config.t Bonsai.t | `From_theme ]
    -> ?extra_attrs:Vdom.Attr.t list Bonsai.t
    -> ?autoclose:Autoclose.t Bonsai.t
    -> ?lock_body_scroll:bool Bonsai.t
    -> ?overflow_auto_wrapper:bool Bonsai.t
    -> ?focus_on_open:bool Bonsai.t
    -> ?restore_focus_on_close:Restore_focus_on_close.t Bonsai.t
    -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> local_ Bonsai.graph
    -> unit
end
