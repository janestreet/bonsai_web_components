open! Core
module Vdom := Virtual_dom.Vdom

module Decoration : sig
  (** A [Decoration.t] is a description of a visual modification that can be applied to
      some text span. *)

  type t

  (** Creates a decoration. All un-specified optional parameters will have the property
      inherited from the parent element. *)
  val create : ?color:Css_gen.Color.t -> unit -> t
end

(** Create a fancy text-box! The fancy text-box is fancy in two ways:
    1. It can grow to fit the size of its contents, but is still responsive to constraints
    2. You can colorize any part of the text, e.g. for syntax highlighting

    - [?min_width] will be applied to the textbox's css. Defaults to `12ch.
    - [?caret_color] sets the color of the caret. Sadly this value can not be inferred
      from the text color. Defaults to "black".
    - [?container_attrs] are attributes applied to the wrapping element. Use these to
      style the textbox (e.g. to make it fill its parent instead of the default
      fit-content width), since the [<textarea>] and highlight [<pre>] inherit most of
      their layout from the wrapper.
    - [?textarea_attrs] are extra attributes applied to the underlying [<textarea>]. Use
      these to attach behaviour that must live on the input element itself, such as an
      [id], focus/blur handlers, or key handlers.
    - [?on_click], if given, is called when the textbox is clicked, with [byte_offset]:
      the UTF-8 byte offset into [text] that the click landed on. The highlight [<pre>]
      sits behind the [<textarea>] and can't be clicked directly, so this is how a caller
      makes rendered regions (e.g. links) interactive: map [byte_offset] back onto the
      regions it produced in [process]. The raw event is passed too, e.g. to inspect
      modifier keys or [preventDefault].
    - [?placeholder] / [?disabled] are forwarded to the underlying [<textarea>].
    - [~text] is the raw text contained in the textbox. You must track this state
      yourself.
    - [~set_text] is the setter that the textbox will call when its content is updated.
    - [~process] is the syntax highlighting function. More details below:

    The [process] function is given the raw text, and is responsible for breaking that
    text up into "decoratable" regions of text. These decoratable regions are expected to
    contain all of the non-whitespace characters that existed in the input text.

    If you don't want syntax highlighting, you can provide
    [fun s -> [s, Decoration.create()]] to the [process] arg. *)
val create
  :  ?min_width:Css_gen.Length.t
  -> ?caret_color:Css_gen.Color.t
  -> ?container_attrs:Vdom.Attr.t list
  -> ?textarea_attrs:Vdom.Attr.t list
  -> ?on_click:
       (byte_offset:int
        -> Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t
        -> unit Vdom.Effect.t)
  -> ?placeholder:string
  -> ?disabled:bool
  -> text:string
  -> set_text:(string -> unit Vdom.Effect.t)
  -> process:(string -> (string * Decoration.t) list)
  -> unit
  -> Vdom.Node.t

module Expert : sig
  (** Much like the non-expert [create] function. The only difference is that the
      [process] function returns a list of vdom nodes directly. You are responsible for
      ensuring that all characters from the input string are precisely emitted. *)
  val create
    :  ?min_width:Css_gen.Length.t
    -> ?caret_color:Css_gen.Color.t
    -> ?container_attrs:Vdom.Attr.t list
    -> ?textarea_attrs:Vdom.Attr.t list
    -> ?on_click:
         (byte_offset:int
          -> Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t
          -> unit Vdom.Effect.t)
    -> ?placeholder:string
    -> ?disabled:bool
    -> text:string
    -> set_text:(string -> unit Vdom.Effect.t)
    -> process:(string -> Vdom.Node.t list)
    -> unit
    -> Vdom.Node.t
end
