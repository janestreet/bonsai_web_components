open! Core
open Codemirror

module type Model = sig
  type t [@@deriving sexp_of]
end

(** An easy-to-use function for making a codemirror textbox with autocompletion
    based on a potentially dynamic sexp-grammar. This is a good way to get
    started using Codemirror - if you want a more complex configuration, you
    can use other functions in this module then. *)
val with_extension
  :  ?extra_extension:Codemirror.State.Extension.t
       (** [extra_extension] defaults to [Basic_setup.basic_setup] *)
  -> ?include_non_exhaustive_hint:bool (** see [autocomplete_extension_of_sexp_grammar] *)
  -> name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> 'a Sexp_grammar.t Bonsai.t
  -> Bonsai.graph
  -> Bonsai_web_ui_codemirror.t Bonsai.t

val extension
  :  ?include_non_exhaustive_hint:bool
       (** Whether to add an extra autocomplete entry indicating that the autocomplete list is
        not exhaustive. The default is [true]. *)
  -> 'a Sexp_grammar.t
  -> State.Extension.t

module Private : sig
  module For_tests : sig
    module Completion : sig
      type t =
        { from : int
        ; to_ : int option
        ; options : string list
        ; exhaustive : bool
        }
      [@@deriving sexp_of]
    end

    val completions
      :  text:string
      -> cursor_position:int
      -> grammar:'a Sexp_grammar.t
      -> Completion.t
  end
end
