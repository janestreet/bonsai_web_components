open! Core
open Codemirror_incremental_sexp_grammar

(** This code parses the next n lines of a sexp according to a sexp grammar.
    We've factored it out so it's easier to test. *)

module Action : sig
  type 'doc t =
    | Parse_some_lines of 'doc
    | Edit_starting_at_nth_line of int
end

(** The [Doc] module type is an interface based on Codemirror's [Text.t], which we factored
    out so we can write OCaml expect tests. *)
module type Doc = sig
  module Line_iterator : sig
    type in_progress

    (* The [string] contained in a [In_progress] is the "current" line's value.
       Empty lines are represented by empty strings. *)
    type t =
      | Done
      | In_progress of (in_progress * string)

    val next : in_progress -> t
  end

  type t

  val iter_lines : t -> start_line:int -> Line_iterator.t
end

module Grammared : sig
  (** A [Grammared.t] manages the tokenization and parsing of a sexp according to a sexp
      grammar. *)
  type ('doc, 'error) t

  val create
    :  ?checkpoint_every_n_lines:int
    -> error_conv:(Syntax_error.t -> 'error)
         (** [error_conv] allows us to convert errors to the format in which they will be
             reported. Doing this on error creation is a lot cheaper than doing it every
              time we get the set of errors. *)
    -> (module Doc with type t = 'doc)
    -> Sexp_grammar.grammar
    -> ('doc, 'error) t

  (** Returns an unordered list of errors parsed so far. *)
  val errors_unsorted : (_, 'error) t -> 'error list

  (** Re-tokenizes part of the doc starting at some line, and returns the parsed tokens.

      Returns [None] if there's no suitable checkpoint to start at, and this task would
      require parsing > 300 lines. *)
  val adhoc_tokenization
    :  ('doc, 'error) t
    -> starting_at_line:int
    -> until_line:int
    -> doc:'doc
    -> Tokenizer.Token.t list option

  (** [is_done] returns [true] if the entire sexp has been parsed.*)
  val is_done : _ t -> bool

  val apply_action : ('doc, 'error) t -> 'doc Action.t -> ('doc, 'error) t
end

module Ungrammared : sig
  (** An [Ungrammared.t] manages the tokenization of a sexp, without a sexp grammar. *)
  type ('doc, 'error) t

  val create
    :  ?checkpoint_every_n_lines:int
    -> error_conv:(Syntax_error.t -> 'error)
    -> (module Doc with type t = 'doc)
    -> ('doc, 'error) t

  (** Returns an unordered list of errors parsed so far. *)
  val errors_unsorted : (_, 'error) t -> 'error list

  val adhoc_tokenization
    :  ('doc, _) t
    -> starting_at_line:int
    -> until_line:int
    -> doc:'doc
    -> Tokenizer.Token.t list option

  (** [is_done] returns [true] if the entire sexp has been parsed.*)
  val is_done : _ t -> bool

  val apply_action : ('doc, 'error) t -> 'doc Action.t -> ('doc, 'error) t
end
