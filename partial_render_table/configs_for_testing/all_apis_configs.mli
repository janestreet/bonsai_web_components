open! Core
open! Bonsai_web

module Prt_output : sig
  type t =
    { view : Vdom.Node.t
    ; range : int * int
    ; inject : int Sharable.Navigation_action.t -> unit Effect.t
    }
end

(** The [All_apis_configs] module is intended for comparing all combinations of dynamic vs
    static columns, [Render_cell.t]s, whether stateful components are used in cells, and
    whether column groups are used.

    These power the main, most general set of PRT tests and benchmarks.

    There's a trade-off between testing all possible combinations, and maintaining a
    comprehensible test suite.

    One particularly tricky dimension is [counters_in_cells]. For e.g. dynamic cols, it's
    impossible to instantiate state per-cell, so we need to maintain state as a map of
    values. We could use this same approach for dynamic cells, where we can instantiate
    state per-cell, but that's not the way we expect the API to be used, so we don't
    currently test this. *)

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

type input := (int, Symbol_table.Row.t, Int.comparator_witness) Sharable.Input.t
type computation := (input, Prt_output.t) Bonsai_bench_scenario.compare_computation

val name : t -> string
val computation : t -> computation

(** The first element in the tuple is a name, to be displayed alongside the diff. *)
val pair_for_diff : string * t * t -> string * computation * computation

val build : t -> string * computation
val get_inject : Prt_output.t -> int Sharable.Navigation_action.t -> unit Ui_effect.t

(** [full_power_comparison] compares the most powerful versions of each API. This means:

    - State in cells where possible
    - Disabling constant folding for columns
    - Column groups, if possible *)
val full_power_comparison : (string * computation) list Lazy.t

val full_power_comparison_sorted_for_bench : (string * computation) list Lazy.t
