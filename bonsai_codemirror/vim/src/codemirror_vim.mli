open! Core

(** [create] will create an extension that - when added to your codemirror's extension
    list will make that codemirror have VIM keybindings.

    if [use_system_clipboard] is true, copy pasting using [y]/[p] will use the system
    clipboard instead of vim's separate register clipboard. Defaults to [false]. *)
val create : ?use_system_clipboard:bool -> unit -> Codemirror.State.Extension.t

(** Enters Vim insert mode for a CodeMirror editor. *)
val enter_insert_mode : Codemirror.View.Editor_view.t -> unit

(** Defines a key mapping, e.g. map jj to Esc in insert mode. *)
val map
  :  ?mode:[< `Insert | `Normal | `Visual > `Normal ]
  -> from:string
  -> to_:string
  -> unit
  -> unit

(** Like map, but non-recursive. Note: doesn't support keys that aren't present in the
    default keymap. This is a limitation of the Javascript library we use. See
    https://github.com/replit/codemirror-vim/blob/7c2a24bd7106d24693459cdd5100bfb802a2b723/src/vim.js#L839 *)
val noremap
  :  ?mode:[< `Insert | `Normal | `Visual > `Normal ]
  -> from:string
  -> to_:string
  -> unit
  -> unit

(** Clear all user defined keybindings
    https://github.com/replit/codemirror-vim/blob/7c2a24bd7106d24693459cdd5100bfb802a2b723/src/vim.js#L847 *)
val mapclear : unit -> unit

(* Defines a command for Ex mode *)
val define_ex : name:string -> prefix:string -> (unit -> unit) -> unit
