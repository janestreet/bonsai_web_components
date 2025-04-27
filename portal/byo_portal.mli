open! Core
open! Bonsai_web
open! Js_of_ocaml

(** [component] allows a [Bonsai.graph @ local -> Vdom.Node.t Bonsai.t] to "portal" its
    output as a child of an arbitrary DOM node, rather than including said output in the
    [Vdom.Node.t Bonsai.t] output of your app.

    Portalling is applied via Bonsai lifecycle effects, so portalled content will update
    after the rest of your DOM, albeit within the same frame. Any lifecycle effects
    defined in your computation will run after your content has been portalled / updated.

    The primary use-case is powering [byo_toplayer]'s popovers, modals, and tooltips.
    Portalling has also been used to drive the content of DOM nodes not controlled by your
    Bonsai app; e.g. with Codemirror-controlled tooltips.

    IMPORTANT: [component] will not attempt to detect whether the [parent] element has
    been disconnected. If you portal under a widget, and the widget is destroyed, and then
    recreated, the new widget will be a DIFFERENT root than the old one. Portal roots
    should generally be globally-stored values, NOT the generated contents of widgets.

    [parent] defaults to [global_toplayer_root]. It is called only when the portal is
    intially created. You should only supply this if portalling under DOM controlled by a
    widget. *)
val component
  :  ?parent:(unit -> Dom_html.element Js.t)
  -> (Bonsai.graph @ local -> Vdom.Node.t Bonsai.t)
  -> Bonsai.graph @ local
  -> unit

(* Returns a div, located directly under the root HTML element, where "globally" portalled
   elements should go. *)
val global_toplayer_root : unit -> Dom_html.element Js.t
val ensure_global_toplayer_root_mounted : unit -> unit

module For_testing : sig
  val active_portals : Byo_portal_private.t String.Map.t Bonsai.Expert.Var.t
end
