open! Core
open! Bonsai_web

let override_for_test
  ~(here : [%call_pos])
  ?override_attrs
  ?(override_content = fun () -> Vdom.Node.none)
  actual
  =
  if Am_running_how_js.(am_in_browser_like_api am_running_how)
  then actual
  else (
    let short_node_name =
      String.chop_suffix_if_exists (Filename.basename here.pos_fname) ~suffix:".ml"
    in
    let attr =
      match override_attrs with
      | Some attr -> Vdom.Attr.many attr
      | None ->
        (match actual with
         | Virtual_dom.Vdom.Node.Element e -> Virtual_dom.Vdom.Node.Element.attrs e
         | _ -> Vdom.Attr.empty)
    in
    Vdom.Node.create short_node_name ~attrs:[ attr ] [ override_content () ])
;;
