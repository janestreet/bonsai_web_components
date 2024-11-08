open! Core

(** This library is a collection of sexp-related extensions for codemirror. *)

(** [extension] is a single function that gives you all the sexp-related codemirror
    extensions. You can also use them a la carte via [Autocomplete],
    [Rainbow_parentheses], and [Validation], but then you need to ensure that you are
    passing a consistent grammar.

    [enable_rainbow_parens] and [enable_validation] are [true] by default.

    [enable_autocomplete] is [false] by default, because it performs poorly on large
    sexps.
    *)
val extension
  :  ?enable_rainbow_parens:bool
  -> ?enable_validation:bool
  -> ?enable_autocomplete:bool
  -> 'a Sexp_grammar.t
  -> Codemirror.State.Extension.t

module Autocomplete = Autocomplete
module Rainbow_parentheses = Rainbow_parentheses
module Validation = Validation
