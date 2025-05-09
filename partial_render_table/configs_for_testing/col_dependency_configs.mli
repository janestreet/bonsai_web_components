open! Core
open! Bonsai_web

module Which_column : sig
  type t =
    | First
    | Middle
    | Last
  [@@deriving compare, sexp, enumerate]
end

module Which_api : sig
  type t =
    | Dynamic_cells
    | Dynamic_cols
  [@@deriving compare, sexp, enumerate]
end

type t =
  { which_column : Which_column.t
  ; which_api : Which_api.t
  }
[@@deriving compare, sexp, enumerate]

module Input : sig
  type t =
    { col_dependency : int
    ; data : Symbol_table.Row.t Int.Map.t
    }
end

val name : t -> string
val computation : t -> (Input.t, Vdom.Node.t) Bonsai_bench_scenario.compare_computation

val all_computations
  : (string * (Input.t, Vdom.Node.t) Bonsai_bench_scenario.compare_computation) list
      Lazy.t

val scenarios : (Input.t, Nothing.t) Bonsai_bench_scenario.Scenario.t list
