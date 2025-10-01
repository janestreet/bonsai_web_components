open! Core
open! Bonsai_web
open Virtual_dom
open Codemirror

module Transaction : sig
  type t = State.Editor_state.t -> State.Transaction.t

  val set_lines : string list -> t
end

type t = private
  { view : Vdom.Node.t
  ; state : State.Editor_state.t
  ; send_transaction : (State.Editor_state.t -> State.Transaction.t) -> unit Effect.t
  ; execute_command : View.Command.t -> unit Effect.t
  ; focus : unit Effect.t
  ; blur : unit Effect.t
  }
[@@deriving fields ~getters]

(** [text] retrieves the current contents of the editor. This can be slow if your file is
    very large. *)
val text : t -> string

(** [set_lines] overrides the entire contents of the editor with [lines]. *)
val set_lines : t -> string list -> unit Effect.t

(** A codemirror text editor component integrated into Bonsai. The codemirror reference
    manual can be found at [https://codemirror.net/6/docs/ref/]. *)
val of_initial_state
  :  ?name:string
       (** If [name] is provided, attaches a "data-codemirror-editor=NAME" attribute to
           the widget, so it can be selected in tests. *)
  -> State.Editor_state.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

(** Uses edge-triggering to re-configure the set of extensions whenever the input value
    changes. Any extensions specified in [initial_state] will get overwritten by the new
    set of extensions.

    Tries to minimize the amount of times the editor state is reset due to a change in the
    extensions using the [equal] function as a cutoff.

    The extensions set in the initial state will be overwritten after 1 frame with the
    extensions from [basic_setup] and [compute_extensions]

    If you want to fully understand what is happening, your best bet is to read
    codemirror's documentation and the source code for this function. The interactions
    between everything involved in this function is too complex to fully explain in a doc
    comment. *)
val with_dynamic_extensions
  :  ?basic_setup:[ `Minimal | `Basic | `None ]
       (** A lot of basic codemirror functionality is powered by extensions (undo/redo,
           even the enter key). This adds those extensions by default.

           Defaults to `Minimal. Options correspond to minimalSetup (the bare minimum set
           of things you should need) and basicSetup (basic functionality for a typical
           editor) in https://codemirror.net/docs/ref/#codemirror.basicSetup *)
  -> ?name:string
       (** If [name] is provided, attaches a "data-codemirror-editor=NAME" attribute to
           the widget, so it can be selected in tests. *)
  -> ?sexp_of:('a -> Sexp.t)
       (** If provided, [sexp_of] is forwarded to the underlying [Bonsai.Edge.on_change],
           where it is used for debugging. *)
  -> equal:('a -> 'a -> bool)
  -> initial_state:State.Editor_state.t
  -> compute_extensions:('a -> State.Extension.t list) Bonsai.t
  -> 'a Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

(** Similar functionality as above, but does not provide default extensions and does not
    do any cutoff on the extensions.

    Unlike the non-prime version, this version instantiates the codemirror instance with
    the extensions from [extensions] instead of replacing the extensions after one frame.

    Users are highly encouraged to use cutoffs for the [Bonsai.t]'s that [extensions]
    depend on so that the editor state is not reset more than it needs to be. *)
val with_dynamic_extensions'
  :  name:string
       (** [name] is name of the codemirror editor, so that it can be referred to in
           tests. *)
  -> initial_text:string
  -> extensions:State.Extension.t list Bonsai.t
  -> local_ Bonsai.graph
  -> t Bonsai.t

module For_testing : sig
  val type_id : (Transaction.t -> unit Effect.t) Type_equal.Id.t
  val enable_logging : unit -> unit
  val disable_logging : unit -> unit
  val with_logging : (unit -> unit) -> unit
end

module Private_for_tests : sig
  module Path_and_generation = Path_and_generation
end
