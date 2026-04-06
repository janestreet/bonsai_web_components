open! Core
open! Bonsai_web
open! Js_of_ocaml

let component
  ?test_selector
  ?(indeterminate = false)
  ?(attrs = [])
  ?(disabled = false)
  ?(tab_index = 0)
  ?(disable_space_key_to_toggle_temp_param_for_skyline = false)
  ?(ignore_clicks = false)
  ~checked
  ~on_change
  view
  =
  let toggle = on_change (not checked) in
  let on_keydown =
    match disabled with
    | true -> Vdom.Attr.empty
    | false ->
      Vdom.Attr.on_keydown (fun event ->
        match Dom_html.Keyboard_code.of_event event with
        | Space ->
          (match disable_space_key_to_toggle_temp_param_for_skyline with
           | true -> Effect.Ignore
           | false ->
             Effect.Many [ toggle; (Effect.Prevent_default [@alert "-deprecated"]) ])
        | _ -> Effect.Ignore)
  in
  let on_click =
    Vdom.Attr.on_click (fun _event ->
      match ignore_clicks with
      | true -> Effect.Ignore
      | false -> if disabled then Effect.Ignore else toggle)
  in
  let state =
    match checked, indeterminate with
    | true, false -> `Checked
    | false, false -> `Unchecked
    | true, true | false, true -> `Indeterminate
  in
  let tab_index =
    match disabled with
    | true -> -1
    | false -> tab_index
  in
  let checkbox =
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.tabindex tab_index
        ; on_keydown
        ; on_click
        ; Bonsai.Test_selector.attr_of_opt test_selector
        ; Vdom.Attr.many attrs
        ]
      view
  in
  let override_content () =
    let ascii =
      match state with
      | `Checked -> "[x]"
      | `Unchecked -> "[ ]"
      | `Indeterminate -> "[-]"
    in
    Vdom.Node.text ascii
  in
  Bonsai_web_override_for_test.override_for_test ~override_content checkbox
;;
