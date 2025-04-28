open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Floating_positioning_new
module Styling = Bonsai_web_ui_toplayer_styling
module Position = Position
module Alignment = Alignment
module Offset = Offset
module Anchor = Anchor
module Match_anchor_side = Match_anchor_side

(* In Chrome, adding children to the DOM root results in a whole-document style recalculation,
   which is expensive. *)
let resolve_toplayer_root_at_graph_construction (_graph : Bonsai.graph) =
  Byo_portal.ensure_global_toplayer_root_mounted ()
;;

let arrow_helper = Styling.arrow_helper

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
    Vdom_toplayer.tooltip
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

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

let focus_on_open_attr = function
  | false -> Vdom.Attr.empty
  | true -> Vdom_toplayer.For_bonsai_web_ui_toplayer.focus_popover_on_open
;;

module Close_on_click_outside = struct
  type t =
    | Yes
    | Yes_unless_target_is_popover
    | No
end

module Controls = struct
  open Js_of_ocaml

  type t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }

  let element_contains node other_node =
    Js.Unsafe.meth_call node "contains" [| Js.Unsafe.inject other_node |] |> Js.to_bool
  ;;

  let element_inert (element : Dom_html.element Js.t) =
    element##hasAttribute (Js.string "inert") |> Js.to_bool
  ;;

  let event_target_inside_a_popover (ev : mouse_event) =
    Option.bind
      (ev##.target |> Js_of_ocaml.Js.Opt.to_option)
      ~f:Vdom_toplayer.For_bonsai_web_ui_toplayer.find_nearest_popover_ancestor
    |> Option.is_some
  ;;

  let on_evt_outside ~eff ~root_id ~bonk (ev : mouse_event) =
    match Js_of_ocaml.Dom_html.getElementById_opt root_id with
    | None -> Effect.Ignore
    | Some root ->
      if element_contains root ev##.target || element_inert root
      then Effect.Ignore
      else
        (* The browser doesn't give you an API to detect "clicks outside", so we've
        attached an event listener to the window. It needs to run on [Capture], because
        otherwise, if [stop_propagation] is called on the trigger element, we will never
        detect a click outside.

        However, if you click on the trigger element, the [Capture] window listener will
        schedule a "close" effect, and then the trigger element's [on_click] will schedule
        an "open" effect, and the popover will stay open. This is not what people expect.

        To counteract this, we [bonk] the close effect, so that it will necessarily run
        after the open effect. *)
        bonk (eff ~click_target_was_another_popover:(event_target_inside_a_popover ev))
  ;;

  let on_esc_attrs ~eff ~root_id =
    (* We have to use [stopPropagation] so that we don't close parent popovers. *)
    let run_if_esc ~inner_eff ev =
      match Js_of_ocaml.Dom_html.Keyboard_code.of_event ev with
      | Escape ->
        (match Js_of_ocaml.Dom_html.getElementById_opt root_id with
         | None -> Effect.Ignore
         | Some root when element_inert root -> Effect.Ignore
         | Some _ -> inner_eff)
      | _ -> Effect.Ignore
    in
    [ Vdom.Attr.Global_listeners.keydown
        ~phase:Bubbling
        ~f:(run_if_esc ~inner_eff:(eff ~focus_inside:false))
    ; Vdom.Attr.on_keydown (fun ev ->
        if element_contains ev##.currentTarget ev##.target
        then
          run_if_esc
            ~inner_eff:(Effect.Many [ Effect.Stop_propagation; eff ~focus_inside:true ])
            ev
        else Effect.Ignore)
    ]
  ;;

  (* [event.target] for click events is where the click ended, not where it began.
     So if you mouse down inside of a popover, drag your mouse to outside of it,
     and release, that will register as a "click outside", and potentially close the
     popover.

     We could work around this by closing on mousedown, but this is not what users expect.

     Instead, if the mousedown immediately before a click was inside of the popover,
     the click will not close that popover. *)
  let monitor_mousedown ~root_id graph =
    let last_mousedown_was_inside, set_last_mousedown_was_inside =
      Bonsai.state `Outside graph
    in
    let monitor_mousedowns_attr =
      let%arr set_last_mousedown_was_inside and root_id in
      Vdom.Attr.Global_listeners.mousedown
        ~phase:Vdom.Attr.Global_listeners.Phase.Capture
        ~f:(fun (ev : mouse_event) ->
          match Js_of_ocaml.Dom_html.getElementById_opt root_id with
          | None -> Effect.Ignore
          | Some root ->
            set_last_mousedown_was_inside
              (if element_contains root ev##.target
               then `Inside_self
               else if event_target_inside_a_popover ev
               then `Inside_another_popover
               else `Outside))
    in
    monitor_mousedowns_attr, last_mousedown_was_inside
  ;;

  let listeners ~on_click_outside ~on_right_click_outside ~on_esc graph =
    let root_id = Bonsai.path_id graph in
    let bonk = Bonsai_extra.bonk graph in
    let monitor_mousedowns_attr, last_mousedown_was_inside =
      monitor_mousedown ~root_id graph
    in
    let%sub click, right_click =
      let%arr on_click_outside
      and on_right_click_outside
      and root_id
      and bonk
      and peek_last_mousedown = Bonsai.peek last_mousedown_was_inside graph in
      let build_click_listener ~kind ~f =
        match f with
        | None -> Vdom.Attr.empty
        | Some f ->
          let listener_f =
            match kind with
            | `Click -> Vdom.Attr.Global_listeners.click
            | `Right_click -> Vdom.Attr.Global_listeners.contextmenu
          in
          let close_effect ~click_target_was_another_popover =
            match kind with
            | `Right_click -> f ~click_target_was_another_popover
            | `Click ->
              (match%bind.Effect peek_last_mousedown with
               (* If the click "started" inside the popover, we disregard it because
                 clicking inside, then dragging outside and releasing shouldn't close. *)
               | Inactive | Active `Inside_self -> Effect.Ignore
               | Active `Inside_another_popover ->
                 f ~click_target_was_another_popover:true
               | Active `Outside -> f ~click_target_was_another_popover)
          in
          listener_f
            ~phase:Vdom.Attr.Global_listeners.Phase.Capture
            ~f:(on_evt_outside ~eff:close_effect ~root_id ~bonk)
      in
      ( build_click_listener ~kind:`Click ~f:on_click_outside
      , build_click_listener ~kind:`Right_click ~f:on_right_click_outside )
    in
    let escape =
      match%sub on_esc with
      | None -> return []
      | Some eff ->
        let%arr eff and root_id in
        on_esc_attrs ~eff ~root_id
    in
    let%arr root_id and monitor_mousedowns_attr and click and right_click and escape in
    Vdom.Attr.many
      ([ Vdom.Attr.id root_id; click; right_click; monitor_mousedowns_attr ] @ escape)
  ;;

  let control_attr
    ~close
    ?(close_on_click_outside = return Close_on_click_outside.Yes)
    ?(close_on_right_click_outside = return Close_on_click_outside.No)
    ?(close_on_esc = return true)
    graph
    =
    let build_on_click conf =
      match%sub conf with
      | Close_on_click_outside.No -> return None
      | Yes_unless_target_is_popover ->
        let%arr close in
        Some
          (fun ~click_target_was_another_popover ->
            if click_target_was_another_popover then Effect.Ignore else close)
      | Yes ->
        let%arr close in
        Some (fun ~click_target_was_another_popover:_ -> close)
    in
    let on_click_outside = build_on_click close_on_click_outside in
    let on_right_click_outside = build_on_click close_on_right_click_outside in
    let on_esc =
      match%sub close_on_esc with
      | false -> return None
      | true ->
        let%arr close in
        Some (fun ~focus_inside:_ -> close)
    in
    listeners ~on_click_outside ~on_right_click_outside ~on_esc graph
  ;;

  let create ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc graph =
    let is_open, set_open = Bonsai.state false graph in
    let%sub open_, close =
      let%arr set_open in
      set_open true, set_open false
    in
    ( control_attr
        ~close
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    , { is_open; open_; close } )
  ;;

  module For_external_state = struct
    type t = Vdom.Attr.t

    let create = control_attr
  end
end

let transpose_join_opt v = Bonsai.transpose_opt v |> Bonsai.map ~f:Option.join

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

  module For_external_state = struct
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

    let opt
      ?(config = `From_theme)
      ?(extra_attrs = return [])
      ?(controls = return Vdom.Attr.empty)
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ~overflow_auto_wrapper
      ?(focus_on_open = Bonsai.return false)
      ?(has_arrow = Bonsai.return false)
      ~is_open
      ~content
      graph
      =
      resolve_toplayer_root_at_graph_construction graph;
      match%sub is_open with
      | None -> return Vdom.Attr.empty
      | Some input ->
        let%arr config = resolve_config config graph
        and controls
        and position = Bonsai.transpose_opt position
        and alignment = Bonsai.transpose_opt alignment
        and offset = Bonsai.transpose_opt offset
        and match_anchor_side_length = transpose_join_opt match_anchor_side_length
        and overflow_auto_wrapper
        and focus_on_open
        and has_arrow
        and extra_attrs
        and content = content input graph in
        let default_offset =
          { Offset.main_axis =
              (if has_arrow
               then config.default_main_axis_offset_with_arrow
               else config.default_main_axis_offset)
          ; cross_axis = 0.
          }
        in
        let arrow = if has_arrow then Some config.arrow else None in
        Vdom_toplayer.popover
          ~popover_attrs:
            (config.popover_attrs
             @ (focus_on_open_attr focus_on_open :: controls :: extra_attrs))
          ?position
          ?alignment
          ~offset:(Option.value offset ~default:default_offset)
          ?match_anchor_side_length
          ~overflow_auto_wrapper
          ?arrow
          content
    ;;

    let bool
      ?config
      ?extra_attrs
      ?controls
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ~overflow_auto_wrapper
      ?focus_on_open
      ?has_arrow
      ~is_open
      ~content
      =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt
        ?config
        ?extra_attrs
        ?controls
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~overflow_auto_wrapper
        ?focus_on_open
        ?has_arrow
        ~is_open
        ~content
    ;;

    let unpositioned
      ~extra_attrs
      ?(controls = return Vdom.Attr.empty)
      ~overflow_auto_wrapper
      ?(focus_on_open = Bonsai.return false)
      ~arrow
      ~is_open
      ~content
      graph
      =
      resolve_toplayer_root_at_graph_construction graph;
      let (_ : unit Bonsai.t) =
        match%sub is_open with
        | None -> return ()
        | Some input ->
          Byo_portal.component
            (fun graph ->
              let%arr controls
              and overflow_auto_wrapper
              and focus_on_open
              and extra_attrs
              and arrow
              and content = content input graph in
              Vdom_toplayer.For_bonsai_web_ui_toplayer.popover_custom
                ~popover_attrs:
                  (focus_on_open_attr focus_on_open :: controls :: extra_attrs)
                ~overflow_auto_wrapper
                ?arrow
                ~popover_content:content
                ())
            graph;
          return ()
      in
      ()
    ;;

    let opt_css ~extra_attrs ?controls ?focus_on_open ~is_open ~content graph =
      let extra_attrs =
        let%arr extra_attrs in
        Vdom_toplayer.For_bonsai_web_ui_toplayer.show_on_mount :: extra_attrs
      in
      unpositioned
        ~extra_attrs
        ?controls
        ~overflow_auto_wrapper:(return false)
        ?focus_on_open
        ~arrow:(return None)
        ~is_open
        ~content
        graph
    ;;

    let bool_css ~extra_attrs ?controls ?focus_on_open ~is_open ~content graph =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt_css ~extra_attrs ?controls ?focus_on_open ~is_open ~content graph
    ;;

    let opt_virtual
      ?(config = `From_theme)
      ?(extra_attrs = return [])
      ?controls
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ~overflow_auto_wrapper
      ?focus_on_open
      ?(has_arrow = return false)
      ~is_open
      ~content
      anchor
      graph
      =
      let config = resolve_config config graph in
      let arrow =
        let%arr has_arrow
        and { arrow; _ } = config in
        if has_arrow then Some arrow else None
      in
      let positioning_attr =
        let%arr config
        and position = Bonsai.transpose_opt position
        and alignment = Bonsai.transpose_opt alignment
        and offset = Bonsai.transpose_opt offset
        and match_anchor_side_length = transpose_join_opt match_anchor_side_length
        and has_arrow
        and anchor in
        let default_offset =
          { Offset.main_axis =
              (if has_arrow
               then config.default_main_axis_offset_with_arrow
               else config.default_main_axis_offset)
          ; cross_axis = 0.
          }
        in
        Floating_positioning_new.position_me
          ~prepare:Vdom_toplayer.For_bonsai_web_ui_toplayer.show_popover
          ~arrow_selector:Vdom_toplayer.For_bonsai_web_ui_toplayer.arrow_selector
          ?position
          ?alignment
          ~offset:(Option.value offset ~default:default_offset)
          ?match_anchor_side_length
          anchor
      in
      let extra_attrs =
        let%arr { popover_attrs; _ } = config
        and extra_attrs
        and positioning_attr in
        (positioning_attr :: popover_attrs) @ extra_attrs
      in
      unpositioned
        ~extra_attrs
        ?controls
        ~overflow_auto_wrapper
        ?focus_on_open
        ~arrow
        ~is_open
        ~content
        graph
    ;;

    let bool_virtual
      ?config
      ?extra_attrs
      ?controls
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ~overflow_auto_wrapper
      ?focus_on_open
      ?has_arrow
      ~is_open
      ~content
      anchor
      graph
      =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt_virtual
        ?config
        ?extra_attrs
        ?controls
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~overflow_auto_wrapper
        ?focus_on_open
        ?has_arrow
        ~is_open
        ~content
        anchor
        graph
    ;;
  end

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
    ~overflow_auto_wrapper
    ?focus_on_open
    ?has_arrow
    ~content
    graph
    =
    let control_attr, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs and control_attr in
      control_attr :: extra_attrs
    in
    ( For_external_state.bool
        ?config
        ~extra_attrs
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~overflow_auto_wrapper
        ?focus_on_open
        ?has_arrow
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        graph
    , controls )
  ;;

  let create_css
    ~extra_attrs
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?focus_on_open
    ~content
    graph
    =
    let control_attr, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs and control_attr in
      control_attr :: extra_attrs
    in
    let () =
      For_external_state.bool_css
        ~extra_attrs
        ?focus_on_open
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        graph
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
    ~overflow_auto_wrapper
    ?focus_on_open
    ?has_arrow
    ~content
    anchor
    graph
    =
    let control_attr, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs and control_attr in
      control_attr :: extra_attrs
    in
    let () =
      For_external_state.bool_virtual
        ?config
        ~extra_attrs
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~overflow_auto_wrapper
        ?focus_on_open
        ?has_arrow
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        anchor
        graph
    in
    controls
  ;;
end

module Modal = struct
  module Config = struct
    type t = Bonsai_web_ui_toplayer_styling.Modal.t = { modal_attrs : Vdom.Attr.t list }
  end

  let resolve_config config graph =
    match config with
    | `From_theme ->
      let%arr theme = View.Theme.current graph in
      [ View.For_components.Toplayer.modal_styles theme ]
    | `This_one config ->
      let%arr { Config.modal_attrs } = config in
      modal_attrs
  ;;

  module For_external_state = struct
    let opt
      ?(config = `From_theme)
      ?(extra_attrs = return [])
      ?(controls = return Vdom.Attr.empty)
      ?lock_body_scroll
      ~overflow_auto_wrapper
      ?(focus_on_open = Bonsai.return true)
      ~is_open
      ~content
      graph
      =
      resolve_toplayer_root_at_graph_construction graph;
      let (_ : unit Bonsai.t) =
        match%sub is_open with
        | None -> return ()
        | Some input ->
          Byo_portal.component
            (fun graph ->
              let%arr modal_styles = resolve_config config graph
              and lock_body_scroll = Bonsai.transpose_opt lock_body_scroll
              and overflow_auto_wrapper
              and focus_on_open
              and extra_attrs
              and controls
              and content = content input graph in
              Vdom_toplayer.For_bonsai_web_ui_toplayer.modal
                ~modal_attrs:
                  (modal_styles
                   @ (focus_on_open_attr focus_on_open :: controls :: extra_attrs))
                ?lock_body_scroll
                ~overflow_auto_wrapper
                content)
            graph;
          return ()
      in
      ()
    ;;

    let bool
      ?config
      ?extra_attrs
      ?controls
      ?lock_body_scroll
      ~overflow_auto_wrapper
      ?focus_on_open
      ~is_open
      ~content
      =
      let content (_ : unit Bonsai.t) graph = content graph in
      let is_open =
        match%arr is_open with
        | true -> Some ()
        | false -> None
      in
      opt
        ?config
        ?extra_attrs
        ?controls
        ?lock_body_scroll
        ~overflow_auto_wrapper
        ?focus_on_open
        ~is_open
        ~content
    ;;
  end

  let create
    ?config
    ?(extra_attrs = return [])
    ?(close_on_click_outside = return Close_on_click_outside.Yes_unless_target_is_popover)
    ?close_on_right_click_outside
    ?close_on_esc
    ?lock_body_scroll
    ~overflow_auto_wrapper
    ?focus_on_open
    ~content
    graph
    =
    let control_attr, controls =
      Controls.create
        ~close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let extra_attrs =
      let%arr extra_attrs and control_attr in
      control_attr :: extra_attrs
    in
    let () =
      For_external_state.bool
        ?config
        ~extra_attrs
        ?lock_body_scroll
        ~overflow_auto_wrapper
        ?focus_on_open
        ~is_open:controls.is_open
        ~content:(content ~close:controls.close)
        graph
    in
    controls
  ;;
end

module For_testing = Byo_portal.For_testing
