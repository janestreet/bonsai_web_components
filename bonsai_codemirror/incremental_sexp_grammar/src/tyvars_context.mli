open! Core

type t [@@deriving sexp_of]

val empty : t
val resolve : t -> string -> Sexp_grammar.grammar option
val register_many : t -> string list -> Sexp_grammar.grammar list -> t Or_error.t
val unregister_many : t -> string list -> t Or_error.t

module For_testing : sig
  (** [max_size t] reports the length of the longest list stored in [t].*)
  val max_size : t -> int
end
