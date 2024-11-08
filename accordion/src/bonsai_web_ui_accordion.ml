open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

type t =
  { view : Vdom.Node.t
  ; is_open : bool
  ; open_ : unit Effect.t
  ; close : unit Effect.t
  ; toggle : unit Effect.t
  }

let component
  ?(extra_container_attrs = Bonsai.return [])
  ?(extra_title_attrs = Bonsai.return [])
  ?(extra_title_container_attrs = Bonsai.return [])
  ?(extra_content_attrs = Bonsai.return [])
  ?(click_event_propagation_behavior = return `Allow)
  ~starts_open
  ~title
  ~content
  ()
  (local_ graph)
  =
  let%tydi { state = is_open; set_state = set_is_open; toggle } =
    Bonsai.toggle' ~default_model:starts_open graph
  in
  let%sub open_, close =
    let%arr set_is_open in
    set_is_open true, set_is_open false
  in
  let view =
    let theme = View.Theme.current graph in
    let title_attrs =
      let%arr extra_title_attrs
      and toggle
      and theme
      and is_open
      and click_event_propagation_behavior in
      let constants = View.constants theme in
      [ Style.title
      ; Vdom.Attr.on_click (fun event ->
          let () =
            match click_event_propagation_behavior with
            | `Stop -> Js_of_ocaml.Dom_html.stopPropagation event
            | `Allow -> ()
          in
          toggle)
      ; (if is_open then Style.title_open else Style.title_closed)
      ; Style.Variables.set
          ~border:(Css_gen.Color.to_string_css constants.extreme_primary_border)
          ~fg_text:(Css_gen.Color.to_string_css constants.extreme.foreground)
          ()
      ; Vdom.Attr.many extra_title_attrs
      ]
    in
    let title =
      let%arr is_open and title and extra_title_container_attrs in
      let is_open_attr =
        if is_open then Style.accordion_open else Style.accordion_closed
      in
      [ View.hbox
          ~attrs:extra_title_container_attrs
          ~main_axis_alignment:Start
          ~cross_axis_alignment:Center
          [ Vdom.Node.div
              ~attrs:[ Style.icon_container ]
              [ Vdom.Node.div ~attrs:[ is_open_attr; Style.icon ] [] ]
          ; title
          ]
      ]
    in
    match%sub is_open with
    | false ->
      let%arr theme
      and title
      and title_attrs
      and extra_content_attrs
      and extra_container_attrs in
      View.card'
        theme
        ~content_attrs:[ Style.no_padding; Vdom.Attr.many extra_content_attrs ]
        ~container_attrs:extra_container_attrs
        ~title_attrs
        ~title
        [ (Vdom.Node.none_deprecated [@alert "-deprecated"]) ]
    | true ->
      let content = content graph in
      let%arr theme
      and content
      and title
      and title_attrs
      and extra_container_attrs
      and extra_content_attrs in
      View.card'
        theme
        ~container_attrs:extra_container_attrs
        ~content_attrs:extra_content_attrs
        ~title_attrs
        ~title
        [ content ]
  in
  let%arr view and is_open and open_ and close and toggle in
  { view; is_open; open_; close; toggle }
;;
