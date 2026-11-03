open! Core

(** [create] will create an extension that - when added to your codemirror's extension
    list will make that codemirror have VIM keybindings.

    if [use_system_clipboard] is true, copy pasting using [y]/[p] will use the system
    clipboard instead of vim's separate register clipboard. Defaults to [false]. *)
val create : ?use_system_clipboard:bool -> unit -> Codemirror.State.Extension.t

val is_insert_mode : Codemirror.View.Editor_view.t -> bool
val on_insert_mode_change : (bool -> unit) -> Codemirror.State.Extension.t

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

(** Removes a key mapping, including default ones. E.g. unmapping [<Space>] (which moves
    the cursor right by default) allows using it as a leader key in multi-key mappings
    like [<Space>1]: without unmapping, the default binding full-matches immediately and
    the multi-key mapping never triggers.

    The context must match exactly. [`Normal] passes no context, matching contextless
    defaults and [map ~mode:`Normal] entries. For example,
    [unmap ~mode:`Visual ~from:"<Space>" ()] does not remove the contextless default; use
    [`Normal].

    Caveat: the Javascript library tracks the default/user boundary by count, and
    unmapping a default binding doesn't update that count. So after unmapping a default, a
    subsequent [mapclear] leaves the earliest-added user binding in place. *)
val unmap
  :  ?mode:[< `Insert | `Normal | `Visual > `Normal ]
  -> from:string
  -> unit
  -> unit

(** Clear all user defined keybindings
    https://github.com/replit/codemirror-vim/blob/7c2a24bd7106d24693459cdd5100bfb802a2b723/src/vim.js#L847 *)
val mapclear : unit -> unit

(** Defines a named vim action (https://github.com/replit/codemirror-vim: [defineAction])
    and binds a key sequence (e.g. "<Space>1") directly to it. [name] must not collide
    with an existing action, including built-ins such as [scroll] and [enterInsertMode]: a
    collision silently overwrites the existing action.

    Registration is global, not per editor. Call once during page initialization, not on
    component activation: repeated calls add duplicate keymap entries, and [mapclear] has
    the caveat described under [unmap].

    Unlike [map ~to_:":some-ex-command"], the action runs immediately rather than opening
    the ex command line and waiting for the user to press enter. The binding is active in
    normal and visual mode, but not insert mode. *)
val define_and_map_action : keys:string -> name:string -> (unit -> unit) -> unit

(* Defines a command for Ex mode *)
val define_ex : name:string -> prefix:string -> (unit -> unit) -> unit
