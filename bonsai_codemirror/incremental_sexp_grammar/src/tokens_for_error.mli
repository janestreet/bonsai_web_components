open! Core
module Token = Tokenizer.Token

(** A [Tokens_for_error.t] is the list of tokens affected by an error.
    It is automatically truncated at 12 tokens, so that we avoid excessively long
    error messages. *)
type t [@@deriving sexp_of, compare]

val singleton : Token.t -> t
val create : Token.t -> Token.t list -> t
val push : t -> Token.t -> t
val tokens_maybe_trunc : t -> Token.t Nonempty_list.t
val to_string : t -> string
val first_token : t -> Token.t
val offset_start : t -> int
val offset_end : t -> int
