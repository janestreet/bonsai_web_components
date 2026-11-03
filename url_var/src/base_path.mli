open! Core

(** Handles the path prefix from an explicit HTML [base] element, such as
    [<base href="/graphs/">], when it resolves to a same-origin directory (with a trailing
    slash). Missing or unsupported bases and DOM lookup errors are treated as no prefix. *)

(** Removes the configured base path, preserving the slashes that follow it:
    ["/graphs/" -> "/"], ["/graphs//view" -> "//view"]. Returns [path] unchanged when no
    base path is configured, and raises when the prefix is missing. *)
val remove_exn : string -> string

(** Adds the configured base path to [path]. *)
val prepend : string -> string

module For_testing : sig
  val set : string -> unit
  val clear : unit -> unit
end
