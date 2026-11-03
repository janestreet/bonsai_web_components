open! Core
open Bonsai_web
module View := Bonsai_web_legacy_view

val to_vdom
  :  ?theme:View.Theme.t
  -> ?on_submit:Bonsai_web_form.With_automatic_view.View.submission_options
  -> ?editable:Bonsai_web_form.With_automatic_view.View.editable
  -> Bonsai_web_form.With_automatic_view.View.t
  -> Vdom.Node.t
