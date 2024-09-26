open! Core
open! Codemirror
open Codemirror_incremental_sexp_grammar

val parsed_more : View.View_update.t -> bool
val line_index_of_pos : Text.Text.t -> int -> int

module Parse_driver : sig
  val grammared : Sexp_grammar.grammar -> State.Extension.t
  val ungrammared : State.Extension.t
  val errors_unsorted : State.Editor_state.t -> Lint.Diagnostic.t list

  val adhoc_tokenization
    :  State.Editor_state.t
    -> starting_at_line:int
    -> until_line:int
    -> Tokenizer.Token.t list option
end
