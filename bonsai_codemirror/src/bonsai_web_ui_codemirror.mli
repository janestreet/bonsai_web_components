open! Core
module Bonsai_proc := Bonsai_web.Proc
open! Bonsai_web
open Virtual_dom
open Codemirror

module Transaction : sig
  type t = State.Editor_state.t -> State.Transaction.t

  val set_lines : string list -> t
end

type t

val view : t -> Vdom.Node.t
val state : t -> State.Editor_state.t
val send_transaction : t -> Transaction.t -> unit Effect.t
val focus : t -> unit Effect.t
val blur : t -> unit Effect.t
val execute_command : t -> View.Command.t -> unit Effect.t

(** [text] retrieves the current contents of the editor. This can be slow if your
    file is very large. *)
val text : t -> string

(** [set_lines] overrides the entire contents of the editor with [lines]. *)
val set_lines : t -> string list -> unit Effect.t

(** A codemirror text editor component integrated into Bonsai. The codemirror
    reference manual can be found at [https://codemirror.net/6/docs/ref/].
*)
val of_initial_state
  :  name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> State.Editor_state.t
  -> Bonsai.graph
  -> t Bonsai.t

(** Uses edge-triggering to re-configure the set of extensions whenever the
    input value changes. Any extensions specified in [initial_state] will get
    overwritten by the new set of extensions.

    If you want to fully understand what is happening, your best bet is to read
    codemirror's documentation and the source code for this function. The
    interactions between everything involved in this function is too complex to
    fully explain in a doc comment. *)
val with_dynamic_extensions
  :  ?basic_setup:[ `Minimal | `Basic | `None ]
       (** A lot of basic codemirror functionality is powered by extensions (undo/redo,
           even the enter key). This adds those extensions by default.

           Defaults to `Minimal. Options correspond to minimalSetup (the bare minimum
           set of things you should need) and basicSetup (basic functionality for a typical
           editor) in https://codemirror.net/docs/ref/#codemirror.basicSetup
        *)
  -> (module Bonsai_proc.Model with type t = 'a)
  -> equal:('a -> 'a -> bool)
  -> name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in tests. *)
  -> initial_state:State.Editor_state.t
  -> compute_extensions:('a -> State.Extension.t list) Bonsai.t
  -> 'a Bonsai.t
  -> Bonsai.graph
  -> t Bonsai.t

module For_testing : sig
  val type_id : (Transaction.t -> unit Effect.t) Type_equal.Id.t
end

module Private_for_tests : sig
  module Path_and_generation = Path_and_generation
end
