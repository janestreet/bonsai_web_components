open! Core
open! Virtual_dom
include module type of Generated.Icon

val all : t list
val name : t -> string

val svg
  :  ?size:Css_gen.Length.t
  -> ?color:Css_gen.Color.t
  -> ?attrs:Vdom.Attr.t list
  -> t
  -> Vdom.Node.t

val by_file_extension : (t * Css_gen.Color.t) String.Map.t
