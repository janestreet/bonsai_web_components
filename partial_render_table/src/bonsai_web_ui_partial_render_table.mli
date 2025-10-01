open! Core
open! Bonsai_web
module Sortable = Sortable
module Order = Bonsai_web_ui_partial_render_table_protocol.Order
module Sort_state = Bonsai_web_ui_partial_render_table_protocol.Sort_state
module Sort_kind = Bonsai_web_ui_partial_render_table_protocol.Sort_kind
module Styling := Bonsai_web_ui_partial_render_table_styling

module For_testing : sig
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

module Which_styling : sig
  type t =
    | This_one of Styling.t Bonsai.t
    | From_theme
    | Legacy_unsafe_raw_classnames
end

module Focus_by_row = Focus.By_row
module Focus_by_cell = Focus.By_cell

module Indexed_column_id : sig
  type t [@@deriving equal, sexp]

  val of_int : int -> t
  val to_int : t -> int
end

(** The first step to creating a PRT is specifying the columns.

    You'll need a ['column_id] type, which is typically a hand-written variant, or a GADT
    derived through the [typed_fields] ppx. *)
module Column_structure : sig
  (** A [Columns.Structure.t] defines which columns your table should have, which order
      they should appear in, and if/how they should be grouped.

      A dynamic set of columns allows your columns to change arbitrarily at runtime, but
      will be less performant than a static set. *)
  type 'column_id t

  val flat : 'column_id list -> 'column_id t
  val flat_dynamic : 'column_id list Bonsai.t -> 'column_id t

  type 'column_id structure := 'column_id t

  module Group : sig
    type 'column_id t

    val leaf : 'column_id -> 'column_id t
    val group : label:Vdom.Node.t Bonsai.t -> 'column_id t list -> 'column_id t
    val lift : 'column_id t list -> 'column_id structure
  end

  module Group_dynamic : sig
    type 'column_id t

    val leaf : 'column_id -> 'column_id t
    val group : label:Vdom.Node.t -> 'column_id t list -> 'column_id t
    val lift : 'column_id t list Bonsai.t -> 'column_id structure
  end

  (** Allows you to configure initial widths for each column. Changes to the function
      argument won't do anything after the initial render.

      If you don't care for some fields, consider using [default_initial_width] *)
  val with_initial_widths
    :  'column_id t
    -> f:('column_id -> Css_gen.Length.t) Bonsai.t
    -> 'column_id t

  (** [with_is_resizable] allows you to disable users from manually resizing some columns. *)
  val with_is_resizable : 'column_id t -> f:('column_id -> bool) Bonsai.t -> 'column_id t

  (** By default, columns have a width of 50px. If using auto-resizing columns, this will
      serve as a min width. *)
  val default_initial_width : Css_gen.Length.t
end

module Render_cell : sig
  (** A [Render_cell.t] specifies how the cells in your table should be rendered, as a
      function of the column id, row key, and data.

      [Pure] is 1.5-4x faster than [Stateful_rows], which is 1.5-3x faster than
      [Stateful_cells]. See: [../bench/bin/main.ml].

      Table that don't need stateful components in cells should use [Pure]. Most other
      tables should try to use [Stateful_rows] over [Stateful_cells]. *)
  type ('key, 'data, 'column_id) t =
    | Pure of ('column_id -> 'key -> 'data -> Vdom.Node.t) Bonsai.t
    | Stateful_rows of
        ('key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> ('column_id -> Vdom.Node.t) Bonsai.t)
    | Stateful_cells of
        ('column_id Bonsai.t
         -> 'key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> Vdom.Node.t Bonsai.t)
end

module Basic : sig
  module Focus : sig
    module By_row = Focus.By_row
    module By_cell = Focus.By_cell

    type ('a, 'p, 'k, 'c) t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t }
          -> ('k Focus_by_row.optional, 'k option, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t }
          -> (('k, 'c) By_cell.optional, ('k * 'c) option, 'k, 'c) t
  end

  module Result : sig
    type ('focus, 'key, 'column_id) t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_state : 'column_id Sortable.t
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      (** [set_column_width] cannot set the width of the column smaller than the minimum
          width of the header. *)
      ; column_widths : ('column_id * [ `Px_float of float ]) list Lazy.t
      (** [column_widths] returns the widths of the columns. For hidden columns, it will
          use the last-known width. The list may be empty on the first frame after the
          table has been included in the page. *)
      ; key_rank : 'key -> int option Effect.t
      (** [key_rank] resolves to the index of a key after sorting + filtering, if present
          in the table. *)
      }
    [@@deriving fields ~getters]
  end

  (** New PRTs should use [Columns.build] instead of [Columns.Dynamic_cells],
      [Dynamic_columns], or [Dynamic_experimental]. *)
  module Columns : sig
    type ('key, 'data, 'column_id) t

    val build
      :  ?sorts:
           ('column_id Bonsai.t
            -> local_ Bonsai.graph
            -> ('key, 'data) Sort_kind.t option Bonsai.t)
      -> ('column_id, _) Comparator.Module.t
      -> columns:'column_id Column_structure.t
      -> render_header:
           ('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:('key, 'data, 'column_id) Render_cell.t
      -> ('key, 'data, 'column_id) t

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Sortable

    include
      Old_columns_intf.Basic
      with type ('key, 'data, 'column_id) t := ('key, 'data, 'column_id) t
       and type Indexed_column_id.t = Indexed_column_id.t
  end

  type 'a compare := 'a -> 'a -> int

  (** This is the main UI component for the table content. *)
  val component
    :  ?styling:Which_styling.t
         (** [styling] defaults to [From_theme]. You can use [This of Styling.t] to style
             a PRT without having to go through the theme. *)
    -> ?resize_column_widths_to_fit:bool Bonsai.t
         (** If [resize_column_widths_to_fit] is [true], columns will autoresize to fit
             content. The [set_column_width] effect, and draggable resize UI, will only
             set min-width.

             If [false], columns can be set to any size, but will not autoresize. *)
    -> ?round_column_width:(float -> float)
         (** [round_column_width] will by default round column widths to 2 decimal places.
             This is not a [Bonsai.t] as it's used within the column width tracker state
             machine.

             This argument is useful for rounding the column widths to whole pixel values
             before setting it if we are experiencing issues with subpixels looping and
             constantly firing insignificant width changes

             If required in the future, this can be changed by switching the
             [state_machine] to a [state_machine_with_input], but the functionality
             doesn't seem like it would ever need to be dynamically set *)
    -> ?filter:(key:'key -> data:'data -> bool) Bonsai.t
         (** An optional function may be provided, which filters the rows in the table. *)
    -> ?override_sort:
         ('key compare -> ('key * 'data) compare -> ('key * 'data) compare) Bonsai.t
         (** override_sort is an optional function that transforms the tables current
             sort, taking into account the default-sort and any user-provided sorts that
             they've added by clicking on column headers.

             [override_sort] is also given the comparison function for the key of the
             table, which the overrider can use as a fall-back for when the the ('key *
             'data) comparison function returns 0. *)
    -> ?default_sort:('key * 'data) compare Bonsai.t
         (** An optional function may be provided to sort the table. *)
    -> ?wrap_header:'column_id Sortable.Wrap_header.basic Bonsai.t
         (** [wrap_header] is typically used to add sorting controls to the header of each
             column. It defaults to [Sortable.Wrap_header.clickable_with_icon ()]. *)
    -> ?preload_rows:int
    -> ?extra_row_attrs:('key -> Vdom.Attr.t list) Bonsai.t
         (** [extra_row_attrs] will be added to the themed/functional attrs attached by
             the PRT on each row. In general, styling of the PRT should be done through
             [~styling]. However, this parameter can be used to attach attributes for
             testing. *)
    -> ('key, 'cmp) Comparator.Module.t
    -> focus:('focus, 'presence, 'key, 'column_id) Focus.t
    -> row_height:[ `Px of int ] Bonsai.t
         (** [row_height] is the height of every row in the table. If the row height is
             specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'data, 'column_id) Columns.t
    -> ('key, 'data, 'cmp) Map.t Bonsai.t (** The input data for the table *)
    -> local_ Bonsai.graph
    -> ('focus, 'key, 'column_id) Result.t Bonsai.t
end

module Expert : sig
  (** In the [Basic] module, you pass the component all of the table data at once. In the
      [Expert] module, by contrast, you give it a "collation" -- that is, a filtered,
      sorted, range-restricted window -- of the full table. This can be useful when the
      table data is too large to pass to the client directly, or when you'd like to update
      your table via RPC. *)

  open Incr_map_collate

  module Focus : sig
    module By_row = Focus.By_row
    module By_cell = Focus.By_cell

    type ('a, 'p, 'k, 'c) t = ('a, 'p, 'k, 'c) Focus.Kind.t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t
          (** Row-selection is not required to be inside the viewport, so the selected row
              can be offscreen such that it isn't given to the table component.
              [compute_presence] forces the user to consider if a row is considered
              'focused' or not. *)
          ; compute_presence : 'k option Bonsai.t -> local_ Bonsai.graph -> 'p Bonsai.t
          (** A user might try to focus-by-key a row that has not been filtered out, but
              is not inside the viewport. In that case, [key_rank] will be used as a
              fallback to compute the desired index. If the effect returns `None`, the key
              does not correspond to a row under the current filter conditions, and the
              focus will be a no-op. *)
          ; key_rank : ('k -> int option Effect.t) Bonsai.t
          }
          -> (('k, 'p) Focus_by_row.t, 'p, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t
          ; compute_presence :
              ('k * 'c) option Bonsai.t -> local_ Bonsai.graph -> 'presence Bonsai.t
          ; key_rank : ('k -> int option Effect.t) Bonsai.t
          }
          -> (('k, 'c, 'presence) By_cell.t, 'presence, 'k, 'c) t
  end

  module Result : sig
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      (** [set_column_width] cannot set the width of the column smaller than the minimum
          width of the header. *)
      ; column_widths : ('column_id * [ `Px_float of float ]) list Lazy.t
      }
    [@@deriving fields ~getters]
  end

  (** New PRTs should use [Columns.build] instead of [Columns.Dynamic_cells],
      [Dynamic_columns], or [Dynamic_experimental]. *)
  module Columns : sig
    type ('key, 'data, 'column_id) t

    val build
      :  ('column_id, _) Comparator.Module.t
      -> columns:'column_id Column_structure.t
      -> render_header:
           ('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:('key, 'data, 'column_id) Render_cell.t
      -> ('key, 'data, 'column_id) t

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Sortable

    include
      Old_columns_intf.Expert
      with type ('key, 'data, 'column_id) t := ('key, 'data, 'column_id) t
       and type Indexed_column_id.t = Indexed_column_id.t
  end

  (** [collate] is useful for tests, and other situations where you want to use a
      [Server_side] collated table, but you have all the data on the client. *)
  val collate
    :  ?operation_order:[ `Filter_first | `Sort_first ]
    -> filter_equal:('filter -> 'filter -> bool)
         (** [filter_equal] is used to decide when the filters have actually changed,
             requiring a recomputation of the collation. *)
    -> order_equal:('order -> 'order -> bool)
         (** [order_equal] is used to decide when the sorting params have actually
             changed, requiring a recomputation of the collation. *)
    -> filter_to_predicate:('filter -> (key:'k -> data:'v -> bool) option)
         (** [filter_to_predicate] takes the current set of filters ['filter] and
             optionally returns a function that can apply those filters to each row. When
             [filter_to_predicate] returns [None], no filtering is done. *)
    -> order_to_compare:('order -> ('k, 'v, 'cmp) Compare.t)
         (** [order_to_compare] takes the current set of sort params ['order] and uses the
             [Compare] specification to decide how to apply them. Return [Unchanged] to
             perform no sorting. *)
    -> ('k, 'v, 'cmp) Map.t Bonsai.t
       (** A [Map.t] containing the source for all the table data, pre-collation. *)
    -> ('k, 'filter, 'order) Collate_params.t Bonsai.t
       (** A [Collate_params.t] is a specification for how to perform collation: it's
           where the ['filter], ['order], and rank range are defined. *)
    -> local_ Bonsai.graph
    -> ('k, 'v) Collated.t Bonsai.t * ('k -> int option Effect.t) Bonsai.t

  val component
    :  ?styling:Which_styling.t
    -> ?resize_column_widths_to_fit:bool Bonsai.t
         (** If [resize_column_widths_to_fit] is [true], columns will autoresize to fit
             content. The [set_column_width] effect, and draggable resize UI, will only
             set min-width.

             If [false], columns can be set to any size, but will not autoresize. *)
    -> ?round_column_width:(float -> float)
         (** [round_column_width] will by default round column widths to 2 decimal places.
             This is not a [Bonsai.t] as it's used within the column width tracker state
             machine.

             This argument is useful for rounding the column widths to whole pixel values
             before setting it if we are experiencing issues with subpixels looping and
             constantly firing insignificant width changes

             If required in the future, this can be changed by switching the
             [state_machine] to a [state_machine_with_input], but the functionality
             doesn't seem like it would ever need to be dynamically set *)
    -> ?preload_rows:int
         (** [preload_rows] is the number of rows that are maintained before and after the
             viewport range. This number can have a significant effect on performance: too
             small and scrolling might be choppy; too large and you start to lose some of
             the benefits of partial rendering. *)
    -> ?extra_row_attrs:('key -> Vdom.Attr.t list) Bonsai.t
         (** [extra_row_attrs] will be added to the themed/functional attrs attached by
             the PRT on each row. In general, styling of the PRT should be done through
             [~styling]. However, this parameter can be used to attach attributes for
             testing. *)
    -> ('key, 'cmp) Comparator.Module.t
    -> focus:('focus, 'presence, 'key, 'column_id) Focus.t
    -> row_height:[ `Px of int ] Bonsai.t
         (** [row_height] is the height of every row in the table. If the row height is
             specified to be 0px or less, we instead use 1px. *)
    -> columns:('key, 'row, 'column_id) Columns.t
    -> ('key, 'row) Collated.t Bonsai.t
       (** The collated value is the proper input to the component. You can use
           [Expert.collate] to get a Collated.t value, or do the collation manually on the
           server by using the Incr_map_collate library manually. *)
    -> local_ Bonsai.graph
    -> ('focus, 'column_id) Result.t Bonsai.t
end
