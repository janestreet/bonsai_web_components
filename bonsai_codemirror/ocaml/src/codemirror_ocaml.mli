open! Core

(** [ocaml_stream_parser] performs syntax highlighting treating Jane Street's OCaml
    extensions differently. *)
val ocaml_stream_parser : Codemirror_bindings.Stream_parser.Stream_parser.t
