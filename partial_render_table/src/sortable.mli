open! Core
open! Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol

(** This module allows you to create a state machine for column sorting order, get/update
    the current value, and create header labels via the [Header] submodule's utils. *)

type 'col_id t

val state
  :  ?initial_order:'col_id Order.t Bonsai.t
  -> equal:('col_id -> 'col_id -> bool)
  -> unit
  -> local_ Bonsai.graph
  -> 'col_id t Bonsai.t

val order : 'col_id t -> 'col_id Order.t
val inject : 'col_id t -> 'col_id Order.Action.t -> unit Effect.t

module Wrap_header : sig
  type 'column_id basic =
    'column_id t
    -> is_sortable:('column_id -> bool)
    -> column_id:'column_id
    -> Vdom.Node.t
    -> Vdom.Node.t

  (** Adds a mouse on-click handler that cycles through "Ascending > Descending > None"
      sort states for this column.

      When the combination in [multisort_columns_when] is used, new columns are added to
      the sort order instead of replacing the existing sort. Defaults to [`Shift_click].
      If [`Disabled], clicking on an unsorted column will always replace the existing
      sort. *)
  val clickable_with_icon
    :  ?extra_attrs:Vdom.Attr.t list
    -> ?sort_indicator_attrs:Vdom.Attr.t list
    -> ?multisort_columns_when:
         [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click | `Disabled ]
    -> unit
    -> _ basic

  (** [clickable_no_icon] attaches a click handler, but will not display icon sort
      indicators. *)
  val clickable_no_icon
    :  ?multisort_columns_when:
         [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click | `Disabled ]
    -> unit
    -> _ basic

  (** [none] will not attach a click handler, or display a sort indicator. *)
  val none : _ basic

  (** Like [clickable_with_icon], but with worse icons. *)
  val clickable_with_icon_deprecated
    :  ?multisort_columns_when:
         [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click | `Disabled ]
    -> unit
    -> _ basic
end

module Header : sig
  (** [Custom] contains helpers for building custom [Wrap_header.t]s. *)

  (** Wraps the input label in an HTML element, which also contains an icon that reflects
      the current sort state. *)
  val with_icon
    :  ?extra_attrs:Vdom.Attr.t list
    -> ?sort_indicator_attrs:Vdom.Attr.t list
    -> Vdom.Node.t
    -> Sort_state.t
    -> Vdom.Node.t

  module Expert : sig
    (** Adds a mouse on-click handler that cycles through "Ascending > Descending > None"
        sort states for this column.

        If the [multisort_columns_when] key combo is used, new columns are added to the
        sort order, instead of replacing it. Defaults to [`Shift_click]. If [`Disabled],
        clicking on an unsorted column will always replace the existing sort. *)
    val default_click_handler
      :  ?multisort_columns_when:
           [ `Shift_click | `Ctrl_click | `Shift_or_ctrl_click | `Disabled ]
      -> ?test_selector:Test_selector.t
      -> 'col_id t
      -> column_id:'col_id
      -> sortable:bool
      -> (Sort_state.t -> Vdom.Node.t)
      -> Vdom.Node.t
  end

  module Legacy : sig
    (** Like [with_icon], but with worse icons. *)
    val wrap_with_icon : Vdom.Node.t -> Sort_state.t -> Vdom.Node.t
  end
end
