open! Core

(** [create] will create an extension that - when added to your codemirror's extension
    list will make that codemirror have Emacs keybindings. *)
val create : unit -> Codemirror.State.Extension.t
