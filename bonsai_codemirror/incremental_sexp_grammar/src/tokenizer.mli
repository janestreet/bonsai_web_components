open! Core

module Token_type : sig
  (* We don't emit comments because we ignore them. *)
  type t =
    | Lparen
    | Rparen
    | String of string
  [@@deriving sexp_of, equal, compare]
end

module Token : sig
  type t =
    { token_type : Token_type.t
    ; nesting_level : int
    ; pos : Position.t
    }
  [@@deriving sexp_of, equal, compare]

  val to_sexp_string : t list -> string
end

type t

val initial : t

(* If this character marks the end of one or more tokens, they will be returned here
   alongside the new state. Usually, there will only be either 0 or 1 tokens; however, 2
   is possible, e.g. the last character of `atom)` will finish both tokens `atom` and `)`. *)
val process : t -> char -> Token.t list * Syntax_error.t list * t

(* Processes the end of the input. *)
val process_eoi : t -> Token.t list * Syntax_error.t list

(* Convenience method for processing multiple characters in sequence. *)
val process_string : t -> string -> Token.t list * Syntax_error.t list * t
