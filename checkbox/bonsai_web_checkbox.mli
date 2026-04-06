open! Core
open! Bonsai_web
open! Js_of_ocaml

(** Use this to implement checkboxes or checkbox like behavior (e.g. toggles) by providing
    a view layer via [Vdom.Node.t list].

    Checkbox behavior available out of the box:
    - Focus
    - Space key to toggle
    - Test only rendering

    Note for compatibility with <label> click forwarding you probably need an <input>
    somewhere in the vdom subtree and to use [ignore_clicks]. Without [ignore_clicks] you
    will get 2 events for every click: one on the visual checkbox, and one on the hidden
    input after the click bubbles up to the <label>. *)
val component
  :  ?test_selector:Bonsai.Test_selector.t
  -> ?indeterminate:bool
       (** Note: [indeterminate] is purely a visual change. It has no impact on whether
           the checkbox's value is used in a form submission. That is decided by the
           checked state, regardless of the indeterminate state.

           https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/input/checkbox *)
  -> ?attrs:Vdom.Attr.t list
  -> ?disabled:bool
  -> ?tab_index:int
  -> ?disable_space_key_to_toggle_temp_param_for_skyline:bool
  -> ?ignore_clicks:bool
       (** If specified, no click listener is installed to toggle the state. Use this if
           you are using a <label> with click forwarding to a hidden input and want to not
           interfere with that mechanism.. *)
  -> checked:bool
  -> on_change:(bool -> unit Effect.t)
  -> Vdom.Node.t list
  -> Vdom.Node.t
