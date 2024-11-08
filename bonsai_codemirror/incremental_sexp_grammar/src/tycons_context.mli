open! Core

(* A [Tycon_context.t] is an immutable, copyable data structure that keeps track of the
   currently available tyvars and defns. It will lazily resolve tyvars, which may be
   defined in terms of tyvar from earlier enclosing tycons. This is useful if you are
   unwilling or unable to eagerly unroll tycons. *)

module Tycon : sig
  type t =
    { name : string
    ; args : Sexp_grammar.grammar list
    ; defns : Sexp_grammar.defn list
    }
  [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

type t [@@deriving sexp_of]

val empty : t

(** [resolve t name] can be used to substitute a [Tyvar name] with the corresponding
    grammar. *)
val resolve : t -> string -> Sexp_grammar.grammar Or_error.t

(** [register] should be called when the DFS traversal passes through a tycon. It will
    register the tyvars and defns from the tycon, so they can be accessed via [resolve]
    and [curr_defns]. *)
val register : t -> Tycon.t -> (Sexp_grammar.grammar * t) Or_error.t

(** [curr_defns] return the defns defined by the nearest enclosing tycon. It's used to
    substitute a [Recursive] for a [Tycon]. *)
val curr_defns : t -> Sexp_grammar.defn list option

(** [pop] removes the nearest enclosing tycon's tyvars and defns from the context.
    This is useful if you own the stack used for DFS traversal through the grammar.*)
val pop : t -> t Or_error.t

module For_testing : sig
  val largest_frame_num_tyvars : t -> int
  val num_frames : t -> int
end
