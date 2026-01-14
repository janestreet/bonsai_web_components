open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Byo_toplayer
module Styling = Bonsai_web_ui_toplayer_styling
module Position = Position
module Alignment = Alignment
module Offset = Offset
module Anchor = Anchor
module Close_on_click_outside = Close_on_click_outside
module Match_anchor_side = Match_anchor_side
module Restore_focus_on_close = Restore_focus_on_close

let arrow_helper = Byo_toplayer.arrow_helper

module Tooltip = struct
  module Config = struct
    type t = Bonsai_web_ui_toplayer_styling.Tooltip.t =
      { tooltip_attrs : Vdom.Attr.t list
      ; anchor_attrs : Vdom.Attr.t list
      ; main_axis_offset : float
      ; cross_axis_offset : float
      ; show_delay : Time_ns.Span.t option
      ; hide_grace_period : Time_ns.Span.t option
      ; hoverable_hide_grace_period : Time_ns.Span.t
      ; arrow : Vdom.Node.t option
      }

    let create = Bonsai_web_ui_toplayer_styling.Tooltip.create
  end

  let default_tooltip_attrs =
    {%css|
      background-color: white;
      color: black;
      border: 1px solid black;
      border-radius: 2px;
      padding: 0.2em 0.3em;
    |}
  ;;

  let create
    ?(config = Config.create ~tooltip_attrs:[ default_tooltip_attrs ] ~arrow:None ())
    ?(position = Position.Auto)
    ?(alignment = Alignment.Center)
    ?(hoverable_inside = false)
    content
    =
    Byo_toplayer.tooltip
      ~tooltip_attrs:config.tooltip_attrs
      ~position
      ~alignment
      ~offset:
        { main_axis = config.main_axis_offset; cross_axis = config.cross_axis_offset }
      ~hoverable_inside
      ?show_delay:config.show_delay
      ?hide_grace_period:
        (match hoverable_inside with
         | true -> Some config.hoverable_hide_grace_period
         | false -> config.hide_grace_period)
      ?arrow:config.arrow
      content
    :: config.anchor_attrs
    |> Vdom.Attr.many
  ;;

  let text ?config ?position ?alignment ?hoverable_inside content =
    create ?config ?position ?alignment ?hoverable_inside (Vdom.Node.text content)
  ;;
end

module Autoclose = Byo_toplayer.Autoclose
module Controls = Byo_toplayer.Controls

module Popover = struct
  module Config = struct
    type t = Bonsai_web_ui_toplayer_styling.Popover.t =
      { popover_attrs : Vdom.Attr.t list
      ; default_main_axis_offset : float
      ; default_main_axis_offset_with_arrow : float
      ; arrow : Vdom.Node.t
      }

    let create = Bonsai_web_ui_toplayer_styling.Popover.create
  end

  let resolve_config config theme =
    match config with
    | `From_theme ->
      let%arr theme = View.Theme.current theme in
      let constants = View.constants theme in
      let arrow = View.For_components.Toplayer.popover_arrow theme in
      let default_main_axis_offset = constants.toplayer.popover_default_offset_px in
      let default_main_axis_offset_with_arrow =
        constants.toplayer.popover_with_arrow_default_offset_px
      in
      let popover_attrs = [ View.For_components.Toplayer.popover_styles theme ] in
      { Config.arrow
      ; popover_attrs
      ; default_main_axis_offset
      ; default_main_axis_offset_with_arrow
      }
    | `This_one config -> config
  ;;

  let always_open
    ?(config = `From_theme)
    ?(extra_attrs = return [])
    ?autoclose
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?(has_arrow = Bonsai.return false)
    ~content
    (local_ graph)
    =
    let config = resolve_config config graph in
    let offset =
      let%arr config
      and offset = Bonsai.transpose_opt offset
      and has_arrow in
      let default_offset =
        { Offset.main_axis =
            (if has_arrow
             then config.default_main_axis_offset_with_arrow
             else config.default_main_axis_offset)
        ; cross_axis = 0.
        }
      in
      Option.value offset ~default:default_offset
    in
    let arrow =
      let%arr config and has_arrow in
      if has_arrow then Some config.arrow else None
    in
    let attrs =
      let%arr config and extra_attrs in
      config.popover_attrs @ extra_attrs
    in
    Byo_toplayer.Popover.always_open
      ~attrs
      ?autoclose
      ?position
      ?alignment
      ~offset
      ?match_anchor_side_length
      ?overflow_auto_wrapper
      ?focus_on_open
      ~arrow
      ~content
      graph
  ;;

  let always_open_css ~extra_attrs ?autoclose ?focus_on_open ~content (local_ graph) =
    Byo_toplayer.Popover.always_open_css
      ~attrs:extra_attrs
      ?autoclose
      ?focus_on_open
      ~content
      graph
  ;;

  let always_open_virtual
    ?(config = `From_theme)
    ?(extra_attrs = return [])
    ?autoclose
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?(has_arrow = return false)
    ~content
    anchor
    (local_ graph)
    =
    let config = resolve_config config graph in
    let arrow =
      let%arr has_arrow
      and { arrow; _ } = config in
      if has_arrow then Some arrow else None
    in
    let offset =
      let%arr config
      and offset = Bonsai.transpose_opt offset
      and has_arrow in
      let default_offset =
        { Offset.main_axis =
            (if has_arrow
             then config.default_main_axis_offset_with_arrow
             else config.default_main_axis_offset)
        ; cross_axis = 0.
        }
      in
      Option.value offset ~default:default_offset
    in
    let extra_attrs =
      let%arr config and extra_attrs in
      config.popover_attrs @ extra_attrs
    in
    Byo_toplayer.Popover.always_open_virtual
      ~attrs:extra_attrs
      ?autoclose
      ?position
      ?alignment
      ~offset
      ?match_anchor_side_length
      ?overflow_auto_wrapper
      ?focus_on_open
      ~content
      ~arrow
      anchor
      graph
  ;;

  let create
    ?config
    ?(extra_attrs = return [])
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?has_arrow
    ~content
    (local_ graph)
    =
    let autoclose, controls =
      Byo_toplayer.Private_for_bonsai_web_ui_toplayer.create_controls
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let popover_attr =
      match%sub controls.is_open with
      | true ->
        always_open
          ?config
          ~autoclose
          ~extra_attrs
          ?position
          ?alignment
          ?offset
          ?match_anchor_side_length
          ?overflow_auto_wrapper
          ?focus_on_open
          ?has_arrow
          ~content:(content ~close:controls.close)
          graph
      | false -> return Vdom.Attr.empty
    in
    popover_attr, controls
  ;;

  let create_css
    ~extra_attrs
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?focus_on_open
    ~content
    (local_ graph)
    =
    let autoclose, controls =
      Byo_toplayer.Private_for_bonsai_web_ui_toplayer.create_controls
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let%sub () =
      match%sub controls.is_open with
      | true ->
        always_open_css
          ~extra_attrs
          ~autoclose
          ?focus_on_open
          ~content:(content ~close:controls.close)
          graph;
        return ()
      | false -> return ()
    in
    controls
  ;;

  let create_virtual
    ?config
    ?(extra_attrs = return [])
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?has_arrow
    ~content
    anchor
    (local_ graph)
    =
    let autoclose, controls =
      Byo_toplayer.Private_for_bonsai_web_ui_toplayer.create_controls
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let (_ : unit Bonsai.t) =
      match%sub controls.is_open with
      | true ->
        always_open_virtual
          ?config
          ~autoclose
          ~extra_attrs
          ?position
          ?alignment
          ?offset
          ?match_anchor_side_length
          ?overflow_auto_wrapper
          ?focus_on_open
          ?has_arrow
          ~content:(content ~close:controls.close)
          anchor
          graph;
        return ()
      | false -> return ()
    in
    controls
  ;;
end

module Modal = struct
  module Config = struct
    type t = Bonsai_web_ui_toplayer_styling.Modal.t = { modal_attrs : Vdom.Attr.t list }
  end

  let resolve_config config (local_ graph) =
    match config with
    | `From_theme ->
      let%arr theme = View.Theme.current graph in
      [ View.For_components.Toplayer.modal_styles theme ]
    | `This_one config ->
      let%arr { Config.modal_attrs } = config in
      modal_attrs
  ;;

  let always_open
    ?(config = `From_theme)
    ?(extra_attrs = return [])
    ?autoclose
    ?lock_body_scroll
    ?overflow_auto_wrapper
    ?focus_on_open
    ?restore_focus_on_close
    ~content
    (local_ graph)
    =
    let attrs =
      let%arr config_styles = resolve_config config graph
      and extra_attrs in
      config_styles @ extra_attrs
    in
    Byo_toplayer.Modal.always_open
      ~attrs
      ?autoclose
      ?lock_body_scroll
      ?overflow_auto_wrapper
      ?focus_on_open
      ?restore_focus_on_close
      ~content
      graph
  ;;

  let create
    ?config
    ?(extra_attrs = return [])
    ?(close_on_click_outside = return Close_on_click_outside.Yes_unless_target_is_popover)
    ?close_on_right_click_outside
    ?close_on_esc
    ?lock_body_scroll
    ?overflow_auto_wrapper
    ?focus_on_open
    ?restore_focus_on_close
    ~content
    (local_ graph)
    =
    let autoclose, controls =
      Byo_toplayer.Private_for_bonsai_web_ui_toplayer.create_controls
        ~close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let (_ : unit Bonsai.t) =
      match%sub controls.is_open with
      | true ->
        always_open
          ?config
          ~autoclose
          ~extra_attrs
          ?lock_body_scroll
          ?overflow_auto_wrapper
          ?focus_on_open
          ?restore_focus_on_close
          ~content:(content ~close:controls.close)
          graph;
        return ()
      | false -> return ()
    in
    controls
  ;;
end
