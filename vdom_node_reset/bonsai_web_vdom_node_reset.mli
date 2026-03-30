(** The [Bonsai_web_vdom_node_reset] is for overriding vdom generating functions. The new
    functions are identical to the originals, except they apply a css reset to created
    nodes so they behave identically whether or not the app itself has a css reset
    installed. The reset we apply to nodes is roughly equivalent to the Tailwind
    Preflight.

    - [Node] overrides are for components using Vdom.Node
    - [Html_syntax] overrides is for ppx_html
    - [Reset_attrs] is exposed for when we call into other libraries and want to add the
      preflight reset to nodes. *)
module Node : sig
  include module type of Virtual_dom.Vdom.Node
end

module Html_syntax : sig
  include module type of Virtual_dom.Vdom.Html_syntax.Html_syntax
end

module Reset_attrs : sig
  module Attr := Virtual_dom.Vdom.Attr

  val a : Attr.t
  val abbr : Attr.t
  val button : Attr.t
  val common : Attr.t

  (* button, input, select, optgroup, textarea *)
  val field : Attr.t
  val heading : Attr.t
  val hr : Attr.t
  val img_and_video : Attr.t

  (* ol, ul, menu *)
  val list : Attr.t
  val table : Attr.t

  (* input, textarea *)
  val placeholder : Attr.t

  (* img, svg, video, canvas, audio, iframe, embed, object *)
  val replaced_element : Attr.t
end
