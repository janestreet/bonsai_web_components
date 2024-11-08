open! Core

type t

val create : Sexp_grammar.grammar -> t
val process : t -> Tokenizer.Token.t -> Lint_error.t list * t
val end_of_input_errors : t -> Lint_error.t list

module For_testing : sig
  val stack : t -> string
  val assert_nothing_too_crazy : t -> unit
end
