open! Core

module Kind : sig
  type t =
    | Incorrect_capitalization
    | Possible_spelling_mistake
  [@@deriving sexp_of, compare]
end

type t =
  { kind : Kind.t option
  ; correction : string
  }
[@@deriving sexp_of, compare]

val capitalization : string -> t
val spelling_mistake : string -> t
val other : string -> t
val spellcheck : possible_corrections:string list -> string -> t list
val list_to_string : t list -> string option
