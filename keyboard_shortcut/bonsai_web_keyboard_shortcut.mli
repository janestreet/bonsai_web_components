open! Core
open! Bonsai_web
module Keystroke := Vdom_keyboard.Keystroke

module Osx_behavior : sig
  type t =
    | Do_nothing
    | Treat_meta_as_ctrl
end

module Hint_status : sig
  (** Status of a given shortcut that can be used conditionally draw a label. *)
  type t =
    | Show
    | Hide
  [@@deriving sexp_of, equal]

  (** The data attribute name added to the listener's element indicating when hints should
      be shown. The attribute value is ["active"] when any [Hint_trigger_key] is pressed
      and ["inactive"] otherwise.

      Example CSS to show/hide keyboard hints:
      {v
        [%{Hint_status.inactive_selector}] {
         --kb-hint-display: none;
        }

        [%{Hint_status.active_selector}] {
         --kb-hint-display: inline;
        }

        .my-kb-hint {
           display: var(--kb-hint-display);
        }
      v} *)
  val data_attr_name : string

  (** CSS selector matching listener boundaries when hints should be shown. *)
  val active_selector : string

  (** CSS selector matching listener boundaries when hints should be hidden. *)
  val inactive_selector : string
end

module Hint_trigger_keys : sig
  (** Controls which modifier keys trigger the display of keyboard shortcut hints. *)
  type t =
    { ctrl : bool
    ; alt : bool
    ; shift : bool
    }
  [@@deriving sexp_of, equal]

  (** Default: ctrl, alt and shift trigger hints *)
  val default : t
end

(** Install a keyboard event listener on the given computation's node. This computation
    will then act as a "boundary", collecting keybindings [register]ed inside it.

    Most apps will typically have one listener at the top-level computation (see
    [install_global_listener]), but nesting multiple listeners is possible (e.g. if you
    want to attach keyboard shortcuts to only a single text-input box, you can install a
    keyboard listener on the input box.).

    Listeners that are attached closest (in the DOM tree above the element that triggered
    the keyboard event) take priority when a shortcut matches (i.e. when a shortcut
    matches the event stops bubbling up the DOM tree).

    If multiple matching shortcuts are registered with the same listener, or multiple
    listeners are installed on the same DOM node, all matching shortcuts will be
    triggered.

    If no matching shortcut exists on the nearest boundary, the event will bubble up to a
    parent boundary. *)
val install_listener
  :  ?osx_behavior:Osx_behavior.t (** Default: [Treat_meta_as_ctrl] *)
  -> ?hint_trigger_keys:Hint_trigger_keys.t (** Default: [Hint_trigger_keys.default] *)
  -> (Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** Install the app's global keyboard listener. Unlike [install_listener], the event
    handlers are attached to the window object, so shortcuts registered inside fire
    regardless of what is focused.

    Install at most one global listener, at the root of your app:
    - Avoid nesting a global listener within a local one: the registrations would collect
      into the global listener, but any local listener closer in the DOM would still take
      priority when a shortcut matches.
    - Avoid installing more than one global listener. *)
val install_global_listener
  :  ?osx_behavior:Osx_behavior.t (** Default: [Treat_meta_as_ctrl] *)
  -> ?hint_trigger_keys:Hint_trigger_keys.t (** Default: [Hint_trigger_keys.default] *)
  -> (Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> Bonsai.graph
  -> Vdom.Node.t Bonsai.t

(** Register a keyboard shortcut with the nearest installed listener

    Shortcuts are only active while their computation is active. This means that you can
    use e.g. [match%sub] to conditionally enable / disable shortcuts.

    If there is no listener installed then this is a noop. *)
val register
  :  ?prevent_default:bool Bonsai.t
  -> effect:unit Effect.t Bonsai.t
  -> Keystroke.t Bonsai.t
  -> Bonsai.graph
  -> unit

(** Register multiple keyboard shortcuts with the nearest installed listener. Each entry
    in the list associates a keystroke with its effect. All shortcuts share the same
    [prevent_default] setting and are activated/deactivated together.

    This is significantly cheaper than calling [register] in a loop or via [Bonsai.assoc],
    because it uses a single [Dynamic_scope.lookup], a single [lifecycle], and a single
    [on_change] regardless of how many keystrokes are registered. *)
val register_many
  :  ?prevent_default:bool Bonsai.t
  -> (Keystroke.t * unit Effect.t) list Bonsai.t
  -> Bonsai.graph
  -> unit

(** Returns the Hint_status based on whether any modifiers are pressed. *)
val hint_status : Bonsai.graph -> Hint_status.t Bonsai.t

module Expert : sig
  (** A lower level version of [install_listener]. It differs in that it creates an
      accessible Attr without inserting it into the VDOM. The Attr can be retrieved via
      [get_event_handler_for_nearest_installation] *)
  val install_listener
    :  ?osx_behavior:Osx_behavior.t (** Default: [Treat_meta_as_ctrl] *)
    -> ?hint_trigger_keys:Hint_trigger_keys.t (** Default: [Hint_trigger_keys.default] *)
    -> (Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.graph
    -> 'a Bonsai.t

  (** Like [install_global_listener], but for a computation that doesn't produce a
      [Vdom.Node.t]. The generated Attr must still be placed in the VDOM (via
      [get_event_handler_for_nearest_installation]) for the window-level handlers to be
      active. *)
  val install_global_listener
    :  ?osx_behavior:Osx_behavior.t (** Default: [Treat_meta_as_ctrl] *)
    -> ?hint_trigger_keys:Hint_trigger_keys.t (** Default: [Hint_trigger_keys.default] *)
    -> (Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.graph
    -> 'a Bonsai.t

  (** An even lower level version of [install_listener]. It enables ergonomic manual
      dispatching of shortcut events. *)
  val install_manual_listener
    :  ?osx_behavior:Osx_behavior.t (** Default: [Treat_meta_as_ctrl] *)
    -> ?hint_trigger_keys:Hint_trigger_keys.t (** Default: [Hint_trigger_keys.default] *)
    -> (Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.graph
    -> 'a Bonsai.t * (Keystroke.t -> unit Effect.t) Bonsai.t

  (** Returns the Attr event handler generated from the nearest installed listener. *)
  val get_event_handler_for_nearest_installation : Bonsai.graph -> Vdom.Attr.t Bonsai.t
end

module For_testing : sig
  (** Run a function with a specific platform and automatically restore the original. Best
      used with `let%with.tilde` *)
  val with_platform : [ `Mac | `Linux | `Windows ] -> f:(unit -> 'a) -> 'a
end
