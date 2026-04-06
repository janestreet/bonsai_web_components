(** {2 Asynchronous API} *)

open! Core
open Js_of_ocaml

(** [copy_text text] stores [text] on the clipboard, overwriting any previous contents. *)
val copy_text : Js.js_string Js.t -> unit Or_error.t Ui_effect.t

(** [read_text] returns the text in the user's clipboard. An error is returned if the user
    doesn't give permission to read their clipboard. *)
val read_text : Js.js_string Js.t Or_error.t Ui_effect.t

(** [copy_blob blobs] stores all [blobs] on the clipboard, overwriting any previous
    contents.

    MIME type of the new clipboard item will be taken from each [blob] and will be used by
    paste target to determine which blob to use. For example, you might use [text/plain]
    and [text/html] to support progressive enhancement when pasting into rich text editors
    or various [image/foo] blobs to support a wide arrange of photo applications. *)
val copy_blob : File.blob Js.t list -> unit Or_error.t Ui_effect.t
