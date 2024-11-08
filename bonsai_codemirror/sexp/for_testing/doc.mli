open! Core
include Codemirror_sexp_kernel.Parsing.Doc

val create : string -> t
val to_lines : t -> string list
