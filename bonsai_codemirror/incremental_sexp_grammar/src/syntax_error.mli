open! Core

type t =
  { error : string
  ; position : Position.t
  }
[@@deriving fields ~getters, sexp_of, compare]

val of_error : Error.t -> position:Position.t -> t
val to_string_referencing_input : input_string:string -> t -> string
val to_string : t -> string
val compare_positions : t -> t -> int
