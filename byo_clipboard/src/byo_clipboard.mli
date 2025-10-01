open! Core
open! Bonsai_web

(** An effect that copies the given text to the clipboard. *)
val copy_text : string -> unit Effect.t

(** An effect that copies the given text as [text/html] to the clipboard. *)
val copy_html : string -> unit Effect.t

(** An effect that copies a link to the clipboard using both text/html (<a> tag) and
    text/plain MIME types. This provides rich formatting where supported with plain text
    fallback. *)
val copy_link : url:string -> title:string -> unit Effect.t

(** [With_status] variants of all the copy functions. These return whether a copy was
    recently performed so UI components can display a status indicator. *)
module With_status : sig
  type t =
    [ `Idle of unit Effect.t
    | `Copied
    ]

  (** An effect that copies the given text to the clipboard. *)
  val copy_text : string Bonsai.t -> local_ Bonsai.graph -> t Bonsai.t

  (** An effect that copies the given text as [text/html] to the clipboard. *)
  val copy_html : string Bonsai.t -> local_ Bonsai.graph -> t Bonsai.t

  (** An effect that copies a link to the clipboard using both text/html (<a> tag) and
      text/plain MIME types. This provides rich formatting where supported with plain text
      fallback. *)
  val copy_link
    :  url:string Bonsai.t
    -> title:string Bonsai.t
    -> local_ Bonsai.graph
    -> t Bonsai.t

  (** Creates a copy_text factory for use in environments where it would otherwise be
      awkward or impossible to thread graph around.

      By default, the text-to-copy is used as the indentifier for the status. The optional
      [key] parameter allows distinguishing between multiple copy buttons that have the
      same text. *)
  val copy_text_factory : local_ Bonsai.graph -> (?key:string -> string -> t) Bonsai.t
end
