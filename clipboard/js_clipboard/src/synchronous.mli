(** {2 Synchronous API} *)

open! Core
open Js_of_ocaml
open Virtual_dom
open Clipboard

(** Defines Virtual_dom attributes for capturing events related to the system clipboard. *)
module Vdom_attr : sig
  (** Intercepts the `copy` event.

      Usage:
      {v
        Js_clipboard.Vdom_attr.on_copy (fun event ->
        event##.clipboardData##setData
          (Js_of_ocaml.Js.string "text/plain")
          (Js_of_ocaml.Js.string "text");
        Vdom.Effect.Prevent_default)
      v}

      Use of methods [intercept_copy] and [intercept_copy_plain] should be preferred for
      convenience.

      Note that calling the event's setData method must be done inside of callback. This
      can be inconvenient when integrating with incr_dom (as it is practically impossible
      to create a [Copy] action). For one example see [{Incr_dom_list}] with a hacky
      solution to generate the content to be copied within the callback. *)
  val on_copy : (Dom_html.clipboardEvent Js.t -> unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Intercepts the `cut` event.

      Usage:
      {v
        Js_clipboard.Vdom_attr.on_copy (fun event ->
        event##.clipboardData##setData
          (Js_of_ocaml.Js.string "text/plain")
          (Js_of_ocaml.Js.string "text");
        ...;
        Vdom.Effect.Prevent_default)
      v}

      Use of methods [intercept_cut] and [intercept_cut_plain] should be preferred for
      convenience.

      Note that calling the event's setData method must be done inside of callback. This
      can be inconvenient when integrating with incr_dom (as it is practically impossible
      to create a [Cut] action). For one example see [{Incr_dom_list}] with a hacky
      solution to generate the content to be copied within the callback. *)

  val on_cut : (Dom_html.clipboardEvent Js.t -> unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Intercepts the `paste` event.

      Usage:
      {v
        Js_clipboard.Vdom_attr.on_paste (fun event ->
        let text = Js_of_ocaml.Js.to_string
          (event##.clipboardData##getData (Js_of_ocaml.Js.string "text/plain"))
        in
        ...;
        Vdom.Effect.Prevent_default)
      v}

      Use of methods [intercept_paste] and [intercept_paste_plain] should be preferred for
      convenuence.

      Again the paste data has to be read before the callback returns. So to integrate
      with [{Incr_dom}] write e.g.:
      {v
           type action =
             | Paste of string DataType.Map.t

           let view ...
             div [intercept_paste (fun p -> inject (Paste p))] [ ... ]

           let on_action m a =
             match a with
             | Paste p -> ...
      v} *)
  val on_paste : (Dom_html.clipboardEvent Js.t -> unit Vdom.Effect.t) -> Vdom.Attr.t

  (** A function to intercept the `copy` event and supply the text that should be stored
      in the clipboard instead. Writes only the text/plain data type. Prevents the default
      action and stops propagation of the event. *)
  val intercept_copy_plain : (unit -> string) -> Vdom.Attr.t

  (** A function to intercept the `cut` event, supply the text that should be stored in
      the clipboard instead and perform a custom action. Writes only the text/plain data
      type. In addition to scheduling the returned Vdom.Effect, this prevents the default
      action and stops propagation of the event. *)
  val intercept_cut_plain : (unit -> string * unit Vdom.Effect.t) -> Vdom.Attr.t

  (** A function to intercept the `paste` event and perform a custom action. Reads only
      the text/plain data type. In addition to scheduling the returned Vdom.Effect, this
      prevents the default action and stops propagation of the event. *)
  val intercept_paste_plain : (string -> unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Similar to [intercept_copy_plain], but can return content of more types. The
      application that will read the clipboard content may decide which of the provided
      data types to use. *)
  val intercept_copy : (unit -> string Datatype.Map.t * unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Similar to [intercept_cut_plain], but can return content of more types. The
      application that will read the clipboard content may decide which of the provided
      data types to use. *)
  val intercept_cut : (unit -> string Datatype.Map.t * unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Similar to [intercept_paste_plain], but receives the content in all data types that
      are stored in the clipboard. Applications usually use text/plain and text/html. *)
  val intercept_paste : (string Datatype.Map.t -> unit Vdom.Effect.t) -> Vdom.Attr.t

  (** Similar to [intercept_copy] but the handler can return None in which case this
      attribute will have no effect and the default behaviour will not be prevented. *)
  val intercept_copy_opt
    :  (unit -> (string Datatype.Map.t * unit Vdom.Effect.t) option)
    -> Vdom.Attr.t

  (** Similar to [intercept_cut] but the handler can return None in which case this
      attribute will have no effect and the default behaviour will not be prevented. *)
  val intercept_cut_opt
    :  (unit -> (string Datatype.Map.t * unit Vdom.Effect.t) option)
    -> Vdom.Attr.t

  (** Similar to [intercept_paste] but the handler can return None in which case this
      attribute will have no effect and the default behaviour will not be prevented. *)
  val intercept_paste_opt
    :  (string Datatype.Map.t -> unit Vdom.Effect.t option)
    -> Vdom.Attr.t
end
