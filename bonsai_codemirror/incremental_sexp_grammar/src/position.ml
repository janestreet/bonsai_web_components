open! Core

type t =
  { offset_start : int
  ; offset_end : int
  }
[@@deriving sexp_of, compare, equal]
