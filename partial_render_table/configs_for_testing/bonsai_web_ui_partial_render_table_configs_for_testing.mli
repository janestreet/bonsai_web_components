open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Table := Bonsai_web_ui_partial_render_table
open Bonsai_perf_shared

(** The purpose of this module is to generate PRTs with various configurations so that
    we can compare them in benchmarks, comparison reports, and tests.

    There's a trade-off between testing all possible combinations, and maintaining a
    comprehensible test suite.

    One particularly tricky dimension is [counters_in_cells]. For e.g. dynamic cols, it's
    impossible to instantiate state per-cell, so we need to maintain state as a map of
    values. We could use this same approach for dynamic cells, where we can instantiate
    state per-cell, but that's not the way we expect the API to be used, so we don't
    currently test this. *)

module Row : sig
  type t =
    { symbol : string
    ; edge : float
    ; max_edge : float
    ; bsize : int
    ; bid : float
    ; ask : float
    ; asize : int
    }

  include Comparator.S with type t := t

  val of_int : int -> t
  val init_rows : int -> t Int.Map.t
  val many_random : int -> t Int.Map.t
end

module type S = sig
  type column_id

  val first_column : column_id
  val all : local_ Bonsai.graph -> (int, Row.t, column_id) Table.Expert.Columns.t

  val with_column_groups
    :  local_ Bonsai.graph
    -> (int, Row.t, column_id) Table.Expert.Columns.t
end

(** An [Action.t] represents the possible actions that can be performed on a partial render
    table. *)
module Action : sig
  type 'key t =
    | Unfocus
    | Focus_up
    | Focus_down
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus_first_column of 'key
    | Focus_index_first_column of int
  [@@deriving sexp, equal]
end

module Prt_input : sig
  (** An [Input.t] packages up all of the inputs to the partial render table and provides
    facilities for modifying individual components. *)
  type ('key, 'data, 'cmp) t

  (** [create] produces a [t], with defaults for most components of the input.

      [filter] defaults to [None]
      [order] defaults to [Compare.Unchanged]
      [rank_range] defaults to [Which_range.To 100]
      [key_range] defaults to [Which_range.All_rows]
      [resize_column_widths_to_fit] defaults to [false]
      [row_height] defaults to [`Px 30]
  *)
  val create
    :  ?filter:(key:'a -> data:'b -> bool) option
    -> ?order:('a, 'b, 'c) Incr_map_collate.Compare.t
    -> ?rank_range:int Incr_map_collate.Collate.Which_range.t
    -> ?key_range:'a Incr_map_collate.Collate.Which_range.t
    -> ?resize_column_widths_to_fit:bool
    -> ?row_height:[ `Px of int ]
    -> ('a, 'b, 'c) Base.Map.t
    -> ('a, 'b, 'c) t

  (** [apply_filter] produces an interaction to change the current filter. *)
  val apply_filter
    :  ('key, 'data, 'cmp) t Input.t
    -> (key:'key -> data:'data -> bool)
    -> 'action Interaction.t

  (** [clear_filter] produces an interaction to remove the current filter. *)
  val clear_filter : _ t Input.t -> 'action Interaction.t

  (** [update_map] produces an interaction to change the map whose data is being rendered in
    the table. *)
  val update_map
    :  ('key, 'data, 'cmp) t Input.t
    -> f:(('key, 'data, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t)
    -> 'action Interaction.t

  (** [set_order] produces an interaction to change the current ordering. *)
  val set_order
    :  ('key, 'data, 'cmp) t Input.t
    -> ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    -> 'action Interaction.t

  (** [set_rank_range] produces an interaction to change the currently visible rank range. *)
  val set_rank_range
    :  _ t Input.t
    -> int Incr_map_collate.Collate.Which_range.t
    -> 'action Interaction.t

  (** [scroll] generates an interaction with abs(start-stop) [change_input]s, which set the
    [rank_range]'s low end to the values between [start] (inclusive) and [stop]
    (exclusive), keeping [window_size] elements in the range. *)
  val scroll
    :  _ t Input.t
    -> start:int
    -> stop:int
    -> window_size:int
    -> 'action Interaction.t
end

module Prt_output : sig
  type t =
    { view : Vdom.Node.t
    ; range : int * int
    ; inject : int Action.t -> unit Effect.t
    }
end

module Config : sig
  module Render_cell_kind : sig
    type t =
      | Pure
      | Stateful_rows
      | Stateful_cells
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module New_api_cols : sig
    type t =
      | Static
      | Dynamic
      | Dynamic_constant_foldable
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module New_api_params : sig
    type t =
      { counters_in_cells : bool
      ; render_cell_kind : Render_cell_kind.t
      ; cols : New_api_cols.t
      ; col_groups : bool
      ; duplicate_col : bool
      }
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module Which_dynamic_cols : sig
    type t =
      | Counters
      | No_counters
      | No_counters_constant_foldable
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module Dynamic_cells_params : sig
    type t =
      { counters_in_cells : bool
      ; col_groups : bool
      ; duplicate_col : bool
      }
  end

  module Dynamic_cols_params : sig
    type t =
      { which_dynamic_cols : Which_dynamic_cols.t
      ; col_groups : bool
      ; duplicate_col : bool
      }
  end

  module Dynamic_experimental_params : sig
    type t =
      { counters_in_cells : bool
      ; constant_foldable_cols : bool
      }
  end

  type t =
    | New_api of New_api_params.t
    | Dynamic_cells of Dynamic_cells_params.t
    | Dynamic_cols of Dynamic_cols_params.t
    | Dynamic_experimental of Dynamic_experimental_params.t
  [@@deriving equal, compare, sexp_of, enumerate, hash]

  include
    Config
    with type t := t
     and type input = (int, Row.t, Int.comparator_witness) Prt_input.t
     and type output = Prt_output.t
     and type action = int Action.t

  (** [full_power_comparison] compares the most powerful versions of each API. This means:

      - State in cells where possible
      - Disabling constant folding for columns
      - Column groups, if possible *)
  val full_power_comparison : t list
end

val scenarios : (Config.input, Config.action) Scenario.t list
