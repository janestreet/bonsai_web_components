open! Core
open! Bonsai_web

type t =
  { view : Vdom.Node.t
  ; is_open : bool
  ; open_ : unit Effect.t
  ; close : unit Effect.t
  ; toggle : unit Effect.t
  }

val component
  :  ?extra_container_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_title_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_title_container_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_content_attrs:Vdom.Attr.t list Bonsai.t
  -> ?click_event_propagation_behavior:[ `Stop | `Allow ] Bonsai.t
       (** default: [`Allow] *)
  -> starts_open:bool
  -> title:Vdom.Node.t Bonsai.t
  -> content:(local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit
  -> local_ Bonsai.graph
  -> t Bonsai.t
