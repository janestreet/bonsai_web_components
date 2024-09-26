open! Core
module Token = Tokenizer.Token

(** A [Lint_error.t] is a higher-level type that makes it simpler to construct errors
    during sexp parsing. *)

module Contents : sig
  type t =
    | Tokens of Tokens_for_error.t
    | Single_position of int
  [@@deriving sexp_of, compare]
end

type t =
  { error_description : string
  ; corrections : Correction.t list
  ; contents : Contents.t
  }
[@@deriving fields ~getters, sexp_of, compare]

val compare_error_descriptions : t -> t -> int
val to_syntax_error : t -> Syntax_error.t
val of_tokens : ?corrections:Correction.t list -> string -> Tokens_for_error.t -> t
val at_pos : ?corrections:Correction.t list -> string -> pos:int -> t

val at_pos_of_first_token
  :  ?corrections:Correction.t list
  -> string
  -> Tokens_for_error.t
  -> t
