open! Core
open! Bonsai_web
open Bonsai_bench_scenario

(** The [Symbol_table] module contains types and scenarios which can be shared across PRTs
    that use the [Row.t] column type, and an [Int] key. *)

module Row : sig
  (** Basic [Symbol_row.t] type, which we can use for tested PRTs. Not all tests need to
      use this type. *)
  type t =
    { symbol : string
    ; edge : float
    ; max_edge : float
    ; bsize : int
    ; bid : float
    ; ask : float
    ; asize : int
    }
  [@@deriving compare, fields ~fields, sexp, typed_fields]

  include Comparator.S with type t := t

  val of_int : int -> t
  val init_rows : int -> t Int.Map.t
  val many_random : int -> t Int.Map.t
end

val scenarios
  : ( (int, Row.t, Int.comparator_witness) Sharable.Input.t
      , int Sharable.Navigation_action.t )
      Scenario.t
      list

val startup_inputs
  :  int list
  -> (string * (int, Row.t, Base.Int.comparator_witness) Sharable.Input.t) list
