open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Byo_toplayer_private_floating
module Position = Position
module Alignment = Alignment
module Offset = Offset
module Anchor = Anchor
module Match_anchor_side = Match_anchor_side
module Restore_focus_on_close = Byo_toplayer_private_vdom.Restore_focus_on_close

(* In Chrome, adding children to the DOM root results in a whole-document style
   recalculation, which is expensive. *)
let resolve_toplayer_root_at_graph_construction (local_ (_graph : Bonsai.graph)) =
  Byo_portal.ensure_global_toplayer_root_mounted ()
;;

let arrow_helper ~attrs ~arrow_len () =
  let (`Px_float arrow_len) = arrow_len in
  let side_len = `Px_float (arrow_len *. sqrt 2.) in
  Vdom.Node.div
    ~attrs:
      ([%css
         {|
           height: %{side_len#Css_gen.Length};
           width: %{side_len#Css_gen.Length};
           transform: rotate(45deg);
           border-bottom: none;
           border-right: none;
         |}]
       :: attrs)
    []
;;

let tooltip = Byo_toplayer_private_vdom.tooltip
let vdom_popover = Byo_toplayer_private_vdom.popover

type mouse_event = Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t

let focus_on_open_attr = function
  | false -> Vdom.Attr.empty
  | true -> Byo_toplayer_private_vdom.For_byo_toplayer.focus_popover_on_open
;;

module Close_on_click_outside = struct
  open Js_of_ocaml

  type t =
    | Yes
    | Yes_unless_target_is_popover
    | No
    | Custom of (target:Dom_html.element Js.t Js.opt -> [ `Close | `Don't_close ])

  let is_target_inside_a_popover ~target =
    Js.Opt.to_option target
    |> Option.bind
         ~f:Byo_toplayer_private_vdom.For_byo_toplayer.find_nearest_popover_ancestor
    |> Option.is_some
  ;;
end

module Autoclose = struct
  type t = Vdom.Attr.t

  open Js_of_ocaml

  let element_contains node other_node =
    Js.Unsafe.meth_call node "contains" [| Js.Unsafe.inject other_node |] |> Js.to_bool
  ;;

  let element_inert (element : Dom_html.element Js.t) =
    element##hasAttribute (Js.string "inert") |> Js.to_bool
  ;;

  let on_evt_outside ~eff ~root_id (ev : mouse_event) =
    match Js_of_ocaml.Dom_html.getElementById_opt root_id with
    | None -> Effect.Ignore
    | Some root ->
      if element_contains root ev##.target || element_inert root
      then Effect.Ignore
      else eff ~target:ev##.target
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

  (* [event.target] for click events is where the click ended, not where it began. So if
     you mouse down inside of a popover, drag your mouse to outside of it, and release,
     that will register as a "click outside", and potentially close the popover.

     We could work around this by closing on mousedown, but this is not what users expect.

     Instead, if the mousedown immediately before a click was inside of the popover, the
     click will not close that popover. *)
  let monitor_mousedown ~root_id graph =
    let last_mousedown_was_inside, set_last_mousedown_was_inside =
      Bonsai.state `Initial graph
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
               else `Clicked_on ev##.target))
    in
    monitor_mousedowns_attr, last_mousedown_was_inside
  ;;

  let listeners ~on_click_outside ~on_right_click_outside ~on_esc (local_ graph) =
    let root_id = Bonsai.path_id graph in
    let bonk = Bonsai_extra.Effects.bonk graph in
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
          let close_effect ~target:click_event_target =
            match kind with
            | `Right_click ->
              (* The browser doesn't give you an API to detect "clicks outside", so we've
                 attached an event listener to the window. It needs to run on [Capture],
                 because otherwise, if [stop_propagation] is called on the trigger
                 element, we will never detect a click outside.

                 However, if you click on the trigger element, the [Capture] window
                 listener will schedule a "close" effect, and then the trigger element's
                 [on_click] will schedule an "open" effect, and the popover will stay
                 open. This is not what people expect.

                 To counteract this, we [bonk] the close effect, so that it will
                 necessarily run after the open effect.

                 A [bonk] isn't needed for [`Click], because the [peek] used there
                 accomplishes the same result of moving the [close] after the [open].
              *)
              (* We use [click_event_target], because you can't drag on a right click. *)
              bonk (f ~target:click_event_target)
            | `Click ->
              (match%bind.Effect peek_last_mousedown with
               | Inactive | Active `Inside_self ->
                 (* If the click "started" inside the popover, we disregard it because
                    clicking inside, then dragging outside and releasing shouldn't close. *)
                 Effect.Ignore
               | Active `Initial ->
                 (* If we don't have an initial mousedown saved, fall back to the click
                    target. *)
                 f ~target:click_event_target
               | Active (`Clicked_on mousedown_target) -> f ~target:mousedown_target)
          in
          listener_f
            ~phase:Vdom.Attr.Global_listeners.Phase.Capture
            ~f:(on_evt_outside ~eff:close_effect ~root_id)
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

  let create
    ~close
    ?(close_on_click_outside = return Close_on_click_outside.Yes)
    ?(close_on_right_click_outside = return Close_on_click_outside.No)
    ?(close_on_esc = return true)
    (local_ graph)
    =
    let build_on_click conf =
      match%sub conf with
      | Close_on_click_outside.No -> return None
      | Yes_unless_target_is_popover ->
        let%arr close in
        Some
          (fun ~target ->
            if Close_on_click_outside.is_target_inside_a_popover ~target
            then Effect.Ignore
            else close)
      | Yes ->
        let%arr close in
        Some (fun ~target:_ -> close)
      | Custom f ->
        let%arr f and close in
        Some
          (fun ~target ->
            match f ~target with
            | `Close -> close
            | `Don't_close -> Effect.Ignore)
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
end

module Controls = struct
  type t =
    { open_ : unit Effect.t Bonsai.t
    ; close : unit Effect.t Bonsai.t
    ; is_open : bool Bonsai.t
    }

  let create
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    (local_ graph)
    =
    let is_open, set_open = Bonsai.state false graph in
    let%sub open_, close =
      let%arr set_open in
      set_open true, set_open false
    in
    ( Autoclose.create
        ~close
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    , { is_open; open_; close } )
  ;;
end

let transpose_join_opt v = Bonsai.transpose_opt v |> Bonsai.map ~f:Option.join

module Popover = struct
  let always_open
    ?(attrs = return [])
    ?(autoclose = return Vdom.Attr.empty)
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?(overflow_auto_wrapper = Bonsai.return false)
    ?(focus_on_open = Bonsai.return false)
    ?(arrow = Bonsai.return None)
    ~content
    (local_ graph)
    =
    resolve_toplayer_root_at_graph_construction graph;
    let%arr autoclose
    and position = Bonsai.transpose_opt position
    and alignment = Bonsai.transpose_opt alignment
    and offset = Bonsai.transpose_opt offset
    and match_anchor_side_length = transpose_join_opt match_anchor_side_length
    and overflow_auto_wrapper
    and focus_on_open
    and arrow
    and attrs
    and content = content graph in
    Byo_toplayer_private_vdom.popover
      ~popover_attrs:(focus_on_open_attr focus_on_open :: autoclose :: attrs)
      ?position
      ?alignment
      ?offset
      ?match_anchor_side_length
      ~overflow_auto_wrapper
      ?arrow
      content
  ;;

  let always_open_unpositioned
    ~attrs
    ?(autoclose = return Vdom.Attr.empty)
    ?overflow_auto_wrapper
    ?(focus_on_open = Bonsai.return false)
    ?(arrow = Bonsai.return None)
    ~content
    (local_ graph)
    =
    resolve_toplayer_root_at_graph_construction graph;
    Byo_portal.component
      (fun graph ->
        let%arr autoclose
        and overflow_auto_wrapper = Bonsai.transpose_opt overflow_auto_wrapper
        and focus_on_open
        and attrs
        and arrow
        and content = content graph in
        Byo_toplayer_private_vdom.For_byo_toplayer.popover_custom
          ~popover_attrs:(focus_on_open_attr focus_on_open :: autoclose :: attrs)
          ?overflow_auto_wrapper
          ?arrow
          ~popover_content:content
          ())
      graph
  ;;

  let always_open_css ~attrs ?autoclose ?focus_on_open ~content (local_ graph) =
    let attrs =
      let%arr attrs in
      (* Other popovers do this in the auto-positioning hook's [prepare]. *)
      Byo_toplayer_private_vdom.For_byo_toplayer.show_on_mount :: attrs
    in
    always_open_unpositioned
      ~attrs
      ?autoclose
      ~overflow_auto_wrapper:(return false)
      ?focus_on_open
      ~arrow:(return None)
      ~content
      graph
  ;;

  let always_open_virtual
    ?(attrs = return [])
    ?autoclose
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?arrow
    ~content
    anchor
    (local_ graph)
    =
    let positioning_attr =
      let%arr position = Bonsai.transpose_opt position
      and alignment = Bonsai.transpose_opt alignment
      and offset = Bonsai.transpose_opt offset
      and match_anchor_side_length = transpose_join_opt match_anchor_side_length
      and anchor in
      Byo_toplayer_private_floating.position_me
        ~prepare:Byo_toplayer_private_vdom.For_byo_toplayer.show_popover
        ~arrow_selector:Byo_toplayer_private_vdom.For_byo_toplayer.arrow_selector
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        anchor
    in
    let attrs =
      let%arr attrs and positioning_attr in
      positioning_attr :: attrs
    in
    always_open_unpositioned
      ~attrs
      ?autoclose
      ?overflow_auto_wrapper
      ?focus_on_open
      ?arrow
      ~content
      graph
  ;;

  let create
    ?attrs
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?arrow
    ~content
    (local_ graph)
    =
    let autoclose, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let popover_attr =
      match%sub controls.is_open with
      | true ->
        always_open
          ?attrs
          ~autoclose
          ?position
          ?alignment
          ?offset
          ?match_anchor_side_length
          ?overflow_auto_wrapper
          ?focus_on_open
          ?arrow
          ~content:(content ~close:controls.close)
          graph
      | false -> return Vdom.Attr.empty
    in
    popover_attr, controls
  ;;

  let create_css
    ~attrs
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?focus_on_open
    ~content
    (local_ graph)
    =
    let autoclose, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let (_ : unit Bonsai.t) =
      match%sub controls.is_open with
      | true ->
        always_open_css
          ~attrs
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
    ?attrs
    ?close_on_click_outside
    ?close_on_right_click_outside
    ?close_on_esc
    ?position
    ?alignment
    ?offset
    ?match_anchor_side_length
    ?overflow_auto_wrapper
    ?focus_on_open
    ?arrow
    ~content
    anchor
    (local_ graph)
    =
    let autoclose, controls =
      Controls.create
        ?close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let (_ : unit Bonsai.t) =
      match%sub controls.is_open with
      | true ->
        always_open_virtual
          ?attrs
          ~autoclose
          ?position
          ?alignment
          ?offset
          ?match_anchor_side_length
          ?overflow_auto_wrapper
          ?focus_on_open
          ?arrow
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
  let always_open
    ?(attrs = return [])
    ?(autoclose = return Vdom.Attr.empty)
    ?lock_body_scroll
    ?overflow_auto_wrapper
    ?(focus_on_open = Bonsai.return true)
    ?(restore_focus_on_close =
      Bonsai.return (Restore_focus_on_close.Yes { prevent_scroll = false }))
    ~content
    (local_ graph)
    =
    resolve_toplayer_root_at_graph_construction graph;
    Byo_portal.component
      (fun graph ->
        let%arr lock_body_scroll = Bonsai.transpose_opt lock_body_scroll
        and overflow_auto_wrapper = Bonsai.transpose_opt overflow_auto_wrapper
        and focus_on_open
        and restore_focus_on_close
        and attrs
        and autoclose
        and content = content graph in
        Byo_toplayer_private_vdom.For_byo_toplayer.modal
          ~modal_attrs:(focus_on_open_attr focus_on_open :: autoclose :: attrs)
          ?lock_body_scroll
          ?overflow_auto_wrapper
          ~restore_focus_on_close
          content)
      graph
  ;;

  let create
    ?attrs
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
      Controls.create
        ~close_on_click_outside
        ?close_on_right_click_outside
        ?close_on_esc
        graph
    in
    let (_ : unit Bonsai.t) =
      match%sub controls.is_open with
      | true ->
        always_open
          ?attrs
          ~autoclose
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

module Private_for_bonsai_web_ui_toplayer = struct
  let create_controls = Controls.create
end
