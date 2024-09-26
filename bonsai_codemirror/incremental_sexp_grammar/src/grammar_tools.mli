open! Core

val strip_tags : 'a Sexp_grammar.with_tag_list -> 'a

(** [record_arg_grammar] parses a record argument. *)
val record_arg_grammar
  :  Sexp_grammar.list_grammar
  -> (Sexp_grammar.grammar option, Error.t) result

(** [to_string] renders the provided string grammar so it can be used in suggestions. *)
val to_string : ?depth:int -> Sexp_grammar.grammar -> string

(** [arg_to_string] calls [to_string], specialized to arguments taken by options,
    variants, and record fields.*)
val arg_to_string : Sexp_grammar.list_grammar -> string
