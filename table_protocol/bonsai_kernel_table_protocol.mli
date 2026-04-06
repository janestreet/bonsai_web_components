@@ portable

open! Core

(** This module re-exports building blocks for tables; see the individual modules for
    documentation. *)

module type Col_id = sig
  type t [@@deriving equal, sexp_of]

  include Comparable.S_plain with type t := t
end

module type Row_id = sig
  type t [@@deriving equal, compare, sexp_of]

  include Comparable.S_plain with type t := t
end

module Sort = Sort
