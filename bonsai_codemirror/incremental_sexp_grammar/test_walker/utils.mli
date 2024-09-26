open! Core

module type S = sig
  type t [@@deriving sexp, sexp_grammar, quickcheck]
end

module Make_sexp_tester (M : S) : sig
  val print_test_t : string -> unit
  val quickcheck_tests : unit -> unit
  val print_grammar : unit -> unit
end

val walk_tokens_and_print_errors : string -> grammar:Sexp_grammar.grammar -> unit

val quickcheck_tests
  :  sexp_of:('a -> Sexp.t)
  -> 'b Sexp_grammar.t
  -> 'a Base_quickcheck.Generator.t
  -> unit
