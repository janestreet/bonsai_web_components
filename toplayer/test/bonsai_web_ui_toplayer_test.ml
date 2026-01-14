open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Node_helpers = Virtual_dom_test_helpers.Node_helpers
module Var = Bonsai.Expert.Var

module Helper = struct
  type positioning =
    { position : Byo_toplayer_private_vdom.Position.t
    ; alignment : Byo_toplayer_private_vdom.Alignment.t
    ; offset : Byo_toplayer_private_vdom.Offset.t
    ; match_anchor_side_length : Byo_toplayer_private_vdom.Match_anchor_side.t option
    }
  [@@deriving sexp_of, equal]

  type popover_inputs =
    { content : Node_helpers.t
    ; arrow : Node_helpers.t option
    ; positioning : positioning option
    ; nested_anchored_popovers : anchored list
    }

  and anchored =
    { inputs : popover_inputs
    ; content_vdom : Vdom.Node.t
    ; arrow_vdom : Vdom.Node.t option
    }

  module Popover_inputs = struct
    type t = popover_inputs =
      { content : Node_helpers.t
      ; arrow : Node_helpers.t option
      ; positioning : positioning option
      ; nested_anchored_popovers : anchored list
      }
  end

  module Anchored_popover = struct
    type t = anchored =
      { inputs : popover_inputs
      ; content_vdom : Vdom.Node.t
      ; arrow_vdom : Vdom.Node.t option
      }

    let inputs { inputs; _ } = inputs
    let content_vdom { content_vdom; _ } = content_vdom

    let best_effort_wrap_content_in_popover_vdom
      { inputs = { positioning; _ }; content_vdom; arrow_vdom }
      =
      match positioning with
      | None -> raise_s [%message "Anchored popovers must have a positioning config"]
      | Some { position; alignment; offset; match_anchor_side_length } ->
        let fake_anchor =
          object%js (_self)
            val tagName = Js_of_ocaml.Js.string "fake_anchor_for_anchored_popover"
          end
        in
        Byo_toplayer_private_vdom.For_testing_byo_toplayer.wrap_anchored_popover
          ~restore_focus_on_close:
            (Byo_toplayer_private_vdom.Restore_focus_on_close.Yes
               { prevent_scroll = false })
          ~overflow_auto_wrapper:false
          ~position
          ~alignment
          ~offset
          ~match_anchor_side_length
          ~content:content_vdom
          ~popover_attrs:[ Vdom.Attr.class_ "currently-untestable" ]
          ~arrow:arrow_vdom
          ~anchor:(Js_of_ocaml.Js.Unsafe.coerce fake_anchor)
    ;;
  end

  module Virtual_popover = struct
    type t =
      { inputs : Popover_inputs.t
      ; popover_vdom : Vdom.Node.t
      }
  end

  module Modal = struct
    type t =
      { vdom : Vdom.Node.t
      ; content : Node_helpers.t
      ; lock_body_scroll : bool
      ; nested_anchored_popovers : anchored list
      }

    let has_attr ~attr_name = function
      | Node_helpers.Text _ | Widget -> false
      | Element { attributes; _ } ->
        List.Assoc.mem attributes ~equal:String.equal attr_name
    ;;

    let is_modal =
      has_attr
        ~attr_name:Byo_toplayer_private_vdom.For_testing_byo_toplayer.modal_attr_name
    ;;

    let lock_body_scroll =
      has_attr
        ~attr_name:
          Byo_toplayer_private_vdom.For_testing_byo_toplayer.lock_body_scroll_attr_name
    ;;
  end

  module Test_selectors = struct
    let app_root = Bonsai_web.Bonsai.Test_selector.make ()
    let anchored_popovers = Bonsai_web.Bonsai.Test_selector.make ()
    let virtual_popovers = Bonsai_web.Bonsai.Test_selector.make ()
    let modals = Bonsai_web.Bonsai.Test_selector.make ()
  end

  module Selectors = struct
    let app_root_id = "app-root-for-tests"
    let anchored_popovers_id = "anchored-popovers-for-tests"
    let virtual_popovers_id = "virtual-popovers-for-tests"
    let modals_id = "modals-for-tests"
  end

  module Result = struct
    type t =
      { anchored_popovers : Anchored_popover.t list
      ; virtual_popovers : Virtual_popover.t list
      ; modals : Modal.t list
      }

    let terrible_handler_hack key value =
      let open Js_of_ocaml in
      let make1 attr_f =
        attr_f (fun evt ->
          Js.Unsafe.fun_call value [| Js.Unsafe.inject evt |] |> Fn.ignore;
          Effect.Ignore)
      in
      let make2 attr_f =
        attr_f (fun evt _ ->
          Js.Unsafe.fun_call value [| Js.Unsafe.inject evt |] |> Fn.ignore;
          Effect.Ignore)
      in
      let strip_on s =
        let len = String.length s in
        if len >= 2 && String.equal (String.sub s ~pos:0 ~len:2) "on"
        then String.sub s ~pos:2 ~len:(len - 2)
        else s
      in
      let open Vdom.Attr in
      match strip_on key with
      | "input" -> on_input |> make2
      | "change" -> on_change |> make2
      | "cancel" -> on_cancel |> make1
      | "click" -> on_click |> make1
      | "toggle" -> on_toggle |> make1
      | "close" -> on_close |> make1
      | "contextmenu" -> on_contextmenu |> make1
      | "dblclick" -> on_double_click |> make1
      | "drag" -> on_drag |> make1
      | "dragstart" -> on_dragstart |> make1
      | "dragend" -> on_dragend |> make1
      | "dragenter" -> on_dragenter |> make1
      | "dragleave" -> on_dragleave |> make1
      | "dragover" -> on_dragover |> make1
      | "drop" -> on_drop |> make1
      | "mousemove" -> on_mousemove |> make1
      | "mouseup" -> on_mouseup |> make1
      | "mousedown" -> on_mousedown |> make1
      | "mouseenter" -> on_mouseenter |> make1
      | "mouseleave" -> on_mouseleave |> make1
      | "mouseover" -> on_mouseover |> make1
      | "mouseout" -> on_mouseout |> make1
      | "keyup" -> on_keyup |> make1
      | "keypress" -> on_keypress |> make1
      | "keydown" -> on_keydown |> make1
      | "scroll" -> on_scroll |> make1
      | "load" -> on_load |> make1
      | "error" -> on_error |> make1
      | "submit" -> on_submit |> make1
      | "pointerdown" -> on_pointerdown |> make1
      | "pointerup" -> on_pointerup |> make1
      | "pointermove" -> on_pointermove |> make1
      | "mousewheel" -> on_mousewheel |> make1
      | "wheel" -> on_wheel |> make1
      | "copy" -> on_copy |> make1
      | "cut" -> on_cut |> make1
      | "paste" -> on_paste |> make1
      | "reset" -> on_reset |> make1
      | "animationend" -> on_animationend |> make1
      | "focus" -> on_focus |> make1
      | "focusin" -> on_focusin |> make1
      | "focusout" -> on_focusout |> make1
      | "blur" -> on_blur |> make1
      | evt_name ->
        raise_s
          [%message
            "BUG: Toplayer test's event simulator does not support this event type"
              evt_name]
    ;;

    let rec node_helpers_to_vdom = function
      | Node_helpers.Text text -> Vdom.Node.text text
      | Widget -> Vdom.Node.create "widget" []
      | Element
          { tag_name
          ; children
          ; key
          ; attributes
          ; string_properties
          ; bool_properties
          ; handlers
          ; styles
          ; hooks
          } ->
        let open Js_of_ocaml in
        Vdom.Node.create
          tag_name
          ?key
          ~attrs:
            (List.map attributes ~f:(fun (k, v) -> Vdom.Attr.create k v)
             @ List.map string_properties ~f:(fun (k, v) ->
               Vdom.Attr.property k (Js.string v |> Js.Unsafe.inject))
             @ List.map bool_properties ~f:(fun (k, v) ->
               Vdom.Attr.property k (Js.bool v |> Js.Unsafe.inject))
             @ List.map handlers ~f:(fun (k, v) -> terrible_handler_hack k v)
             @ List.map styles ~f:(fun (k, v) ->
               Vdom.Attr.create [%string "style.%{k}"] v)
             @ List.map hooks ~f:(fun (k, v) ->
               Vdom.Attr.create
                 [%string "%{k}_hook_ungettable"]
                 (Sexp.to_string_mach (Vdom.Attr.Hooks.For_testing.Extra.sexp_of_t v))))
          (List.map children ~f:node_helpers_to_vdom)
    ;;

    let rec to_vdom ~verbose popover =
      let main_vdom, nested =
        match popover with
        | `Anchored_popover ({ Anchored_popover.inputs; _ } as anchored) ->
          let view =
            if verbose
            then Anchored_popover.best_effort_wrap_content_in_popover_vdom anchored
            else anchored.content_vdom
          in
          view, inputs.nested_anchored_popovers
        | `Virtual_popover { Virtual_popover.inputs; popover_vdom } ->
          let view =
            if verbose then popover_vdom else node_helpers_to_vdom inputs.content
          in
          view, inputs.nested_anchored_popovers
        | `Modal { Modal.vdom; nested_anchored_popovers; content; _ } ->
          let view = if verbose then vdom else node_helpers_to_vdom content in
          view, nested_anchored_popovers
      in
      let tag =
        match popover with
        | `Anchored_popover _ | `Virtual_popover _ -> "popover"
        | `Modal _ -> "modal"
      in
      let nested_vdom =
        match nested with
        | [] ->
          (* We don't want the widget in expect test output. *)
          Vdom.Node.none_deprecated
          [@alert "-deprecated"]
        | nested ->
          List.map nested ~f:(fun x -> to_vdom ~verbose (`Anchored_popover x))
          |> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "nested-popovers-for-tests" ]
      in
      Vdom.Node.create tag [ main_vdom; nested_vdom ]
    ;;

    let wrap_app_vdom
      ?(verbose = false)
      { anchored_popovers; virtual_popovers; modals }
      app_vdom
      =
      let anchored_vdom =
        List.map anchored_popovers ~f:(fun x -> to_vdom ~verbose (`Anchored_popover x))
      in
      let virtual_vdom =
        List.map virtual_popovers ~f:(fun x -> to_vdom ~verbose (`Virtual_popover x))
      in
      let modal_vdom = List.map modals ~f:(fun x -> to_vdom ~verbose (`Modal x)) in
      {%html|
        <html id="html-for-tests">
          <div
            id=%{Selectors.app_root_id}
            %{Bonsai.Test_selector.attr Test_selectors.app_root}
          >
            %{app_vdom}
          </div>
          <div
            id=%{Selectors.anchored_popovers_id}
            %{Bonsai.Test_selector.attr Test_selectors.anchored_popovers}
          >
            *{anchored_vdom}
          </div>
          <div
            id=%{Selectors.virtual_popovers_id}
            %{Bonsai.Test_selector.attr Test_selectors.virtual_popovers}
          >
            *{virtual_vdom}
          </div>
          <div id=%{Selectors.modals_id} %{Bonsai.Test_selector.attr Test_selectors.modals}>
            *{modal_vdom}
          </div>
        </html>
      |}
    ;;
  end

  let rec extract_anchored_single_node node_helper =
    let hook_inputs =
      Node_helpers.get_hook_value_opt
        node_helper
        ~name:Byo_toplayer_private_vdom.For_testing_popover_hook.hook_name
        ~type_id:Byo_toplayer_private_vdom.For_testing_popover_hook.type_id
    in
    match hook_inputs with
    | Some [] ->
      failwith "Invalid state[anchored]: popover hook exists, but contains no popovers."
    | Some inputs ->
      List.map
        inputs
        ~f:
          (fun
            { content; arrow; position; alignment; offset; match_anchor_side_length; _ }
          ->
          let content_helper = Node_helpers.unsafe_convert_exn content in
          { content_vdom = content
          ; arrow_vdom = arrow
          ; inputs =
              { content = content_helper
              ; arrow = Option.map arrow ~f:Node_helpers.unsafe_convert_exn
              ; positioning =
                  Some { position; alignment; offset; match_anchor_side_length }
              ; nested_anchored_popovers = get_all_anchored content_helper
              }
          })
    | None -> []

  and get_all_anchored root_helper =
    let rec loop node_helpers acc =
      match node_helpers with
      | [] -> (* We could reverse, but order doesn't really matter. *) acc
      | Node_helpers.Widget :: tl | Text _ :: tl -> loop tl acc
      | (Element { children; _ } as curr) :: tl ->
        loop (children @ tl) (extract_anchored_single_node curr @ acc)
    in
    loop [ root_helper ] []
  ;;

  let extract_modal root_vdom root_helper =
    let content =
      match root_helper with
      | Node_helpers.Element
          { children = [ Element { children = [ content ]; _ }; _ ]; _ } -> content
      | Element _ ->
        (* This test has the added benefit of adding a check on the [Popover_dom] changing
           unexpectedly. *)
        failwith
          "modal structure malformed! This test is likely out of date with the current \
           implementation of [Popover_dom]."
      | _ -> failwith "modal root must be an element"
    in
    Some
      { Modal.vdom = root_vdom
      ; content
      ; lock_body_scroll = Modal.lock_body_scroll root_helper
      ; nested_anchored_popovers = get_all_anchored content
      }
  ;;

  (* Virtual popovers being opened / closed is controlled by Bonsai computations, which
     are active / inactive independently of vdom. Therefore, we can always just pull them
     from the Var that backs portalling. *)
  let extract_virtual root_vdom root_helper =
    let positioning =
      let%map.Option hook_inputs =
        Node_helpers.get_hook_value_opt
          root_helper
          ~name:Byo_toplayer_private_floating.For_testing_position_me_hook.hook_name
          ~type_id:Byo_toplayer_private_floating.For_testing_position_me_hook.type_id
      in
      { position = hook_inputs.position
      ; alignment = hook_inputs.alignment
      ; offset = hook_inputs.offset
      ; match_anchor_side_length = hook_inputs.match_anchor_side_length
      }
    in
    (* We could probably factor something out for structural tests of modals / popovers,
       but:
       - We expect modals not to have arrows
       - The DOM implementations of modals and popovers may diverse someday *)
    let content, arrow =
      match root_helper with
      | Element
          { children =
              [ Element { children = [ content ]; _ }
              ; Element { children = [ arrow ]; _ }
              ; _
              ]
          ; _
          } -> content, Some arrow
      | Element { children = [ Element { children = [ content ]; _ }; _ ]; _ } ->
        content, None
      | Element _ ->
        (* This test has the added benefit of adding a check on the [Popover_dom] changing
           unexpectedly. *)
        failwith
          "popover structure malformed! This test is likely out of date with the current \
           implementation of [Popover_dom]."
      | _ -> failwith "popover root must be an element"
    in
    Some
      { Virtual_popover.popover_vdom = root_vdom
      ; inputs =
          { content
          ; arrow
          ; positioning
          ; nested_anchored_popovers = get_all_anchored content
          }
      }
  ;;

  let extract_non_anchored = function
    | Vdom.Node.None -> None
    | vdom ->
      let helper = Node_helpers.unsafe_convert_exn vdom in
      (match Modal.is_modal helper with
       | true ->
         let%map.Option modal = extract_modal vdom helper in
         `Modal modal
       | false ->
         let%map.Option virtual_ = extract_virtual vdom helper in
         `Virtual_popover virtual_)
  ;;

  let get_toplayer_elements app_vdom =
    let anchored_popovers =
      let%arr app_vdom in
      let app_helper = Node_helpers.unsafe_convert_exn app_vdom in
      get_all_anchored app_helper
    in
    let virtual_and_modals =
      let%arr portals = Bonsai.Expert.Var.value Byo_portal.For_testing.active_portals in
      Map.data portals
      |> List.map ~f:Byo_portal_private.For_testing.vdom
      |> List.map ~f:extract_non_anchored
      |> List.filter_opt
    in
    let virtual_popovers =
      let%arr virtual_and_modals in
      List.filter_map virtual_and_modals ~f:(function
        | `Virtual_popover x -> Some x
        | `Modal _ -> None)
    in
    let modals =
      let%arr virtual_and_modals in
      List.filter_map virtual_and_modals ~f:(function
        | `Modal x -> Some x
        | `Virtual_popover _ -> None)
    in
    let%arr anchored_popovers and virtual_popovers and modals in
    { Result.anchored_popovers; virtual_popovers; modals }
  ;;

  let wrap_app_vdom ?verbose app_vdom =
    let%arr app_vdom
    and result = get_toplayer_elements app_vdom in
    Result.wrap_app_vdom ?verbose result app_vdom
  ;;
end

module Actions = struct
  type t =
    | Open
    | Close
    | Reset_models
end

module Popover_test_result = struct
  type t =
    | Open of
        { content_html : string
        ; arrow_html : string option
        ; positioning : Helper.positioning option
        }
    | Closed
  [@@deriving sexp_of, equal]

  let conv
    { Helper.Popover_inputs.content; arrow; positioning; nested_anchored_popovers = _ }
    =
    Open
      { content_html = Node_helpers.to_string_html content
      ; arrow_html = Option.map ~f:Node_helpers.to_string_html arrow
      ; positioning
      }
  ;;
end

module Test_result = struct
  type t =
    { result : Helper.Result.t
    ; open_ : unit Effect.t
    ; close : unit Effect.t
    ; reset_models : unit Effect.t
    }
end

open Bonsai_web_ui_toplayer

let test_component ?position ?alignment ?offset ?match_anchor_side_length ~content =
  Bonsai.with_model_resetter' ~f:(fun ~reset:reset_models (local_ graph) ->
    let%tydi popover, { open_ = open_anchored; close = close_anchored; is_open = _ } =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~content
        graph
    in
    let%tydi { Controls.open_ = open_virtual; close = close_virtual; is_open = _ } =
      Popover.create_virtual
        ~overflow_auto_wrapper:(return false)
        ?position
        ?alignment
        ?offset
        ?match_anchor_side_length
        ~content
        (Bonsai.return (Anchor.of_coordinate ~relative_to:`Viewport ~x:0. ~y:0.))
        graph
    in
    let app_vdom =
      let%arr popover in
      Vdom.Node.div ~attrs:[ popover ] []
    in
    let%arr result = Helper.get_toplayer_elements app_vdom
    and open_virtual
    and open_anchored
    and close_virtual
    and close_anchored
    and reset_models in
    { Test_result.result
    ; open_ = Effect.Many [ open_virtual; open_anchored ]
    ; close = Effect.Many [ close_virtual; close_anchored ]
    ; reset_models
    })
;;

module%test [@name "vdom output"] _ = struct
  let toggle_button ~label open_ close is_open =
    let%arr open_ and close and is_open in
    let on_click = if is_open then close else open_ in
    let formatted_id =
      "toggle-" ^ Capitalization.apply_to_words Kebab_case (String.split ~on:' ' label)
    in
    {%html|
      <button id=%{formatted_id} on_click=%{fun _ -> on_click}>
        Toggle popover %{formatted_id#String}
      </button>
    |}
  ;;

  let anchored_with_toggle ~label (local_ graph) =
    let attr, { Controls.open_; close; is_open } =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ -> return {%html|%{label#String}|})
        graph
    in
    attr, toggle_button ~label open_ close is_open
  ;;

  let virtual_with_toggle ~label (local_ graph) =
    let { Controls.open_; close; is_open } =
      Popover.create_virtual
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ -> return {%html|%{label#String}|})
        (Anchor.of_coordinate ~relative_to:`Viewport ~x:0. ~y:0. |> return)
        graph
    in
    toggle_button ~label open_ close is_open
  ;;

  let create_nested_content
    ?(num_anchored = 1)
    ?(num_virtual = 1)
    ~prefix
    ()
    ~close:_
    (local_ graph)
    =
    let anchored_attrs, anchored_togglers =
      List.init num_anchored ~f:(fun i ->
        anchored_with_toggle
          ~label:[%string "%{prefix}Nested Anchored %{i + 1#Int}"]
          graph)
      |> List.unzip
    in
    let virtual_togglers =
      List.init num_virtual ~f:(fun i ->
        virtual_with_toggle ~label:[%string "%{prefix}Nested Virtual %{i + 1#Int}"] graph)
    in
    let%arr anchored_attrs = Bonsai.all anchored_attrs
    and anchored_togglers = Bonsai.all anchored_togglers
    and virtual_togglers = Bonsai.all virtual_togglers in
    let id =
      [%string "%{prefix}-root"] |> String.filter ~f:(fun c -> not (Char.equal c ' '))
    in
    {%html|
      <div>
        <div id=%{id} *{anchored_attrs}></div>
        *{anchored_togglers} *{virtual_togglers}
      </div>
    |}
  ;;

  let test_popovers_and_virtual_popovers (local_ graph) =
    let anchored1, toggle_anchored1 = anchored_with_toggle ~label:"Anchored 1" graph in
    let anchored2, toggle_anchored2 = anchored_with_toggle ~label:"Anchored 2" graph in
    let ( anchored3
        , { Controls.open_ = open_anchored3
          ; close = close_anchored3
          ; is_open = is_open_anchored3
          } )
      =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ~content:(create_nested_content ~prefix:"Anchored " ())
        graph
    in
    let toggle_anchored3 =
      toggle_button ~label:"Anchored 3" open_anchored3 close_anchored3 is_open_anchored3
    in
    let toggle_virtual1 = virtual_with_toggle ~label:"Virtual 1" graph in
    let toggle_virtual2 = virtual_with_toggle ~label:"Virtual 2" graph in
    let { Controls.open_ = open_virtual3
        ; close = close_virtual3
        ; is_open = is_open_virtual3
        }
      =
      Popover.create_virtual
        ~overflow_auto_wrapper:(return false)
        ~content:(create_nested_content ~prefix:"Virtual " ())
        (Anchor.of_coordinate ~relative_to:`Viewport ~x:0. ~y:0. |> return)
        graph
    in
    let toggle_virtual3 =
      toggle_button ~label:"Virtual 3" open_virtual3 close_virtual3 is_open_virtual3
    in
    let app_root =
      let%arr anchored1
      and anchored2
      and anchored3
      and toggle_anchored1
      and toggle_anchored2
      and toggle_anchored3
      and toggle_virtual1
      and toggle_virtual2
      and toggle_virtual3 in
      {%html|
        <div>
          <div
            class="anchored-root"
            %{anchored1}
            %{anchored2}
            %{anchored3}
          ></div>
          %{toggle_anchored1} %{toggle_anchored2} %{toggle_anchored3} %{toggle_virtual1}
          %{toggle_virtual2} %{toggle_virtual3}
        </div>
      |}
    in
    Helper.wrap_app_vdom ~verbose:true app_root
  ;;

  let%expect_test "Multiple and nested" =
    let handle =
      Handle.create (Result_spec.vdom Fn.id) test_popovers_and_virtual_popovers
    in
    Handle.show handle;
    [%expect
      {|
      <html id="html-for-tests">
        <div id="app-root-for-tests">
          <div>
            <div class="anchored-root"> </div>
            <button id="toggle-anchored-1" @on_click>  Toggle popover  toggle-anchored-1 </button>

            <button id="toggle-anchored-2" @on_click>  Toggle popover  toggle-anchored-2 </button>

            <button id="toggle-anchored-3" @on_click>  Toggle popover  toggle-anchored-3 </button>

            <button id="toggle-virtual-1" @on_click>  Toggle popover  toggle-virtual-1 </button>
            <button id="toggle-virtual-2" @on_click>  Toggle popover  toggle-virtual-2 </button>

            <button id="toggle-virtual-3" @on_click>  Toggle popover  toggle-virtual-3 </button>
          </div>
        </div>
        <div id="anchored-popovers-for-tests"> </div>
        <div id="virtual-popovers-for-tests"> </div>
        <div id="modals-for-tests"> </div>
      </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-anchored-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
          <div id="app-root-for-tests">
            <div>
      -|      <div class="anchored-root"> </div>
      +|      <div class="anchored-root" vdom_toplayer_popover=<omitted>> </div>
              <button id="toggle-anchored-1" @on_click>  Toggle popover  toggle-anchored-1 </button>


            </div>
          </div>
      -|  <div id="anchored-popovers-for-tests"> </div>
      +|  <div id="anchored-popovers-for-tests">
      +|    <popover>
      +|      <div popover="manual"
      +|           tabindex="-1"
      +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|           class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
      +|           custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)"))
      +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|           vdom_toplayer_popover_inertness=()
      +|           vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|           style={
      +|             position: absolute;
      +|           }>
      +|        <div class="popover_dom__inline_class_hash_replaced_in_test"> Anchored 1 </div>
      +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|      </div>
      +|    </popover>
      +|  </div>
          <div id="virtual-popovers-for-tests"> </div>
          <div id="modals-for-tests"> </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-anchored-3";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
              </div>
            </popover>
      +|    <popover>
      +|      <div popover="manual"
      +|           tabindex="-1"
      +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|           class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
      +|           custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)"))
      +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|           vdom_toplayer_popover_inertness=()
      +|           vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|           style={
      +|             position: absolute;
      +|           }>
      +|        <div class="popover_dom__inline_class_hash_replaced_in_test">
      +|          <div>
      +|            <div id="Anchored-root"> </div>
      +|            <button id="toggle-anchored-nested-anchored-1" @on_click>  Toggle popover  toggle-anchored-nested-anchored-1 </button>
      +|
      +|            <button id="toggle-anchored-nested-virtual-1" @on_click>  Toggle popover  toggle-anchored-nested-virtual-1 </button>
      +|          </div>
      +|        </div>
      +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|      </div>
      +|    </popover>
          </div>
          <div id="virtual-popovers-for-tests"> </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-anchored-nested-anchored-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
                <div class="popover_dom__inline_class_hash_replaced_in_test">
                  <div>
      -|            <div id="Anchored-root"> </div>
      +|            <div id="Anchored-root" vdom_toplayer_popover=<omitted>> </div>
                    <button id="toggle-anchored-nested-anchored-1" @on_click>  Toggle popover  toggle-anchored-nested-anchored-1 </button>


                <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
              </div>
      +|      <div class="nested-popovers-for-tests">
      +|        <popover>
      +|          <div popover="manual"
      +|               tabindex="-1"
      +|               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|               class="currently-untestable floating_hash_replaced_in_test popover_hash_replaced_in_test"
      +|               custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)"))
      +|               floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|               vdom_toplayer_popover_inertness=()
      +|               vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|               style={
      +|                 position: absolute;
      +|               }>
      +|            <div class="popover_dom__inline_class_hash_replaced_in_test"> Anchored Nested Anchored 1 </div>
      +|            <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|          </div>
      +|        </popover>
      +|      </div>
            </popover>
          </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
            </popover>
          </div>
      -|  <div id="virtual-popovers-for-tests"> </div>
      +|  <div id="virtual-popovers-for-tests">
      +|    <popover>
      +|      <div popover="manual"
      +|           tabindex="-1"
      +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|           id="bonsai_path_replaced_in_test"
      +|           class="default_theme_helpers__inline_class_hash_replaced_in_test default_theme_helpers__inline_class_hash_replaced_in_test floating_hash_replaced_in_test popover_hash_replaced_in_test toplayer_hash_replaced_in_test"
      +|           custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_default_theme_helpers_anon_variable_1_hash_replaced_in_test white)(--ppx_css_default_theme_helpers_anon_variable_2_hash_replaced_in_test black)(--ppx_css_default_theme_helpers_anon_variable_3_hash_replaced_in_test 1px)(--ppx_css_default_theme_helpers_anon_variable_4_hash_replaced_in_test grey))
      +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|           @on_click_global
      +|           @on_keydown_global
      +|           @on_mousedown_global
      +|           vdom_toplayer_popover_inertness=()
      +|           vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|           @on_keydown
      +|           style={
      +|             position: fixed;
      +|           }>
      +|        <div class="popover_dom__inline_class_hash_replaced_in_test"> Virtual 1 </div>
      +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|      </div>
      +|    </popover>
      +|  </div>
          <div id="modals-for-tests"> </div>
        </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-3";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
              </div>
            </popover>
      +|    <popover>
      +|      <div popover="manual"
      +|           tabindex="-1"
      +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|           id="bonsai_path_replaced_in_test"
      +|           class="default_theme_helpers__inline_class_hash_replaced_in_test default_theme_helpers__inline_class_hash_replaced_in_test floating_hash_replaced_in_test popover_hash_replaced_in_test toplayer_hash_replaced_in_test"
      +|           custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_default_theme_helpers_anon_variable_1_hash_replaced_in_test white)(--ppx_css_default_theme_helpers_anon_variable_2_hash_replaced_in_test black)(--ppx_css_default_theme_helpers_anon_variable_3_hash_replaced_in_test 1px)(--ppx_css_default_theme_helpers_anon_variable_4_hash_replaced_in_test grey))
      +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|           @on_click_global
      +|           @on_keydown_global
      +|           @on_mousedown_global
      +|           vdom_toplayer_popover_inertness=()
      +|           vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|           @on_keydown
      +|           style={
      +|             position: fixed;
      +|           }>
      +|        <div class="popover_dom__inline_class_hash_replaced_in_test">
      +|          <div>
      +|            <div id="Virtual-root"> </div>
      +|            <button id="toggle-virtual-nested-anchored-1" @on_click>  Toggle popover  toggle-virtual-nested-anchored-1 </button>
      +|
      +|            <button id="toggle-virtual-nested-virtual-1" @on_click>  Toggle popover  toggle-virtual-nested-virtual-1 </button>
      +|          </div>
      +|        </div>
      +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|      </div>
      +|    </popover>
          </div>
          <div id="modals-for-tests"> </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-nested-virtual-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
              </div>
            </popover>
      +|    <popover>
      +|      <div popover="manual"
      +|           tabindex="-1"
      +|           data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|           id="bonsai_path_replaced_in_test"
      +|           class="default_theme_helpers__inline_class_hash_replaced_in_test default_theme_helpers__inline_class_hash_replaced_in_test floating_hash_replaced_in_test popover_hash_replaced_in_test toplayer_hash_replaced_in_test"
      +|           custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)")(--ppx_css_default_theme_helpers_anon_variable_1_hash_replaced_in_test white)(--ppx_css_default_theme_helpers_anon_variable_2_hash_replaced_in_test black)(--ppx_css_default_theme_helpers_anon_variable_3_hash_replaced_in_test 1px)(--ppx_css_default_theme_helpers_anon_variable_4_hash_replaced_in_test grey))
      +|           floating_positioning_virtual=((prepare <fun>)(position Auto)(alignment Center)(offset((main_axis 0)(cross_axis 0)))(match_anchor_side_length())(arrow_selector([data-floating-ui-arrow-parent]))(anchor <anchor>))
      +|           @on_click_global
      +|           @on_keydown_global
      +|           @on_mousedown_global
      +|           vdom_toplayer_popover_inertness=()
      +|           vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
      +|           @on_keydown
      +|           style={
      +|             position: fixed;
      +|           }>
      +|        <div class="popover_dom__inline_class_hash_replaced_in_test"> Virtual Nested Virtual 1 </div>
      +|        <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
      +|      </div>
      +|    </popover>
          </div>
          <div id="modals-for-tests"> </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-nested-anchored-1";
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-3";
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-virtual-1";
    Handle.recompute_view_until_stable handle
  ;;

  let modal_with_toggle ~label (local_ graph) =
    let { Controls.open_; close; is_open } =
      Modal.create
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ -> return {%html|%{label#String}|})
        graph
    in
    toggle_button ~label open_ close is_open
  ;;

  let test_modals (local_ graph) =
    let toggle_modal1 = modal_with_toggle ~label:"Modal 1" graph in
    let toggle_modal2 = modal_with_toggle ~label:"Modal 2" graph in
    let { Controls.open_ = open_modal3; close = close_modal3; is_open = is_open_modal3 } =
      Modal.create
        ~overflow_auto_wrapper:(return false)
        ~content:(create_nested_content ~prefix:"Modal " ())
        graph
    in
    let toggle_modal3 =
      toggle_button ~label:"Modal 3" open_modal3 close_modal3 is_open_modal3
    in
    let app_root =
      let%arr toggle_modal1 and toggle_modal2 and toggle_modal3 in
      {%html|<div>%{toggle_modal1} %{toggle_modal2} %{toggle_modal3}</div>|}
    in
    Helper.wrap_app_vdom app_root
  ;;

  let%expect_test "Multiple and nested" =
    let handle = Handle.create (Result_spec.vdom Fn.id) test_modals in
    Handle.show handle;
    [%expect
      {|
      <html id="html-for-tests">
        <div id="app-root-for-tests">
          <div>
            <button id="toggle-modal-1" @on_click>  Toggle popover  toggle-modal-1 </button>

            <button id="toggle-modal-2" @on_click>  Toggle popover  toggle-modal-2 </button>

            <button id="toggle-modal-3" @on_click>  Toggle popover  toggle-modal-3 </button>
          </div>
        </div>
        <div id="anchored-popovers-for-tests"> </div>
        <div id="virtual-popovers-for-tests"> </div>
        <div id="modals-for-tests"> </div>
      </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
          <div id="anchored-popovers-for-tests"> </div>
          <div id="virtual-popovers-for-tests"> </div>
      -|  <div id="modals-for-tests"> </div>
      +|  <div id="modals-for-tests">
      +|    <modal> Modal 1 </modal>
      +|  </div>
        </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-3";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
          <div id="modals-for-tests">
            <modal> Modal 1 </modal>
      +|    <modal>
      +|      <div>
      +|        <div id="Modal-root"> </div>
      +|        <button id="toggle-modal-nested-anchored-1" @on_click>  Toggle popover  toggle-modal-nested-anchored-1 </button>
      +|
      +|        <button id="toggle-modal-nested-virtual-1" @on_click>  Toggle popover  toggle-modal-nested-virtual-1 </button>
      +|      </div>
      +|    </modal>
          </div>
        </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-nested-anchored-1";
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:2 handle;
    [%expect
      {|
            <modal>
              <div>
      -|        <div id="Modal-root"> </div>
      +|        <div id="Modal-root" vdom_toplayer_popover_hook_ungettable="<omitted>"> </div>
                <button id="toggle-modal-nested-anchored-1" @on_click>  Toggle popover  toggle-modal-nested-anchored-1 </button>

                <button id="toggle-modal-nested-virtual-1" @on_click>  Toggle popover  toggle-modal-nested-virtual-1 </button>
              </div>
      +|      <div class="nested-popovers-for-tests">
      +|        <popover> Modal Nested Anchored 1 </popover>
      +|      </div>
            </modal>
          </div>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-nested-anchored-1";
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-3";
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#toggle-modal-1";
    Handle.recompute_view_until_stable handle
  ;;

  module Modal_test_result = struct
    type t =
      { app_root : Vdom.Node.t
      ; modal_desc : Sexp.t
      }

    let sexp_of_t { modal_desc; _ } = modal_desc
    let get_vdom { app_root; _ } = app_root
  end

  let%expect_test "modals pass through lock_body_scroll" =
    let lock_body_scroll = Var.create false in
    let component (local_ graph) =
      let { Controls.open_; close; is_open } =
        Modal.create
          ~overflow_auto_wrapper:(return false)
          ~lock_body_scroll:(Var.value lock_body_scroll)
          ~content:(fun ~close:_ _ -> return {%html|Hi|})
          graph
      in
      let toggle =
        let%arr open_ and close and is_open in
        if is_open then close else open_
      in
      let app_root =
        let%arr toggle in
        {%html|<button id="toggle-button" on_click=%{fun _ -> toggle}>Toggle</button>|}
      in
      let result = Helper.get_toplayer_elements app_root in
      let modal_desc =
        match%arr result with
        | { modals = []; _ } -> [%sexp "No Modals"]
        | { modals = [ { lock_body_scroll; _ } ]; _ } ->
          [%message "One modal" (lock_body_scroll : bool)]
        | _ -> [%sexp "More than one modal"]
      in
      let%arr modal_desc and app_root in
      { Modal_test_result.modal_desc; app_root }
    in
    let handle = Handle.create (Result_spec.sexp (module Modal_test_result)) component in
    Handle.show handle;
    [%expect {| "No Modals" |}];
    Handle.click_on ~get_vdom:Modal_test_result.get_vdom handle ~selector:"#toggle-button";
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| ("One modal" (lock_body_scroll false)) |}];
    Var.set lock_body_scroll true;
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| ("One modal" (lock_body_scroll true)) |}];
    Handle.click_on ~get_vdom:Modal_test_result.get_vdom handle ~selector:"#toggle-button";
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| "No Modals" |}]
  ;;

  let%expect_test "CSS-positioned popover doesn't break tests" =
    let handle =
      Handle.create (Result_spec.vdom Fn.id) (fun (local_ graph) ->
        let view =
          let%tydi { open_; close; _ } =
            Popover.create_css
              ~extra_attrs:(Bonsai.return [])
              ~content:(fun ~close:_ __FILE__ ->
                return (Vdom.Node.div [ Vdom.Node.text "popover" ]))
              graph
          in
          let%arr open_ and close in
          {%html|
            <div>
              <button on_click=%{fun _ -> open_} id="open">Open</button
              ><button on_click=%{fun _ -> close} id="close">Close</button>
            </div>
          |}
        in
        Helper.wrap_app_vdom ~verbose:true view)
    in
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#open";
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      <html id="html-for-tests">
        <div id="app-root-for-tests">
          <div>
            <button id="open" @on_click> Open </button>
            <button id="close" @on_click> Close </button>
          </div>
        </div>
        <div id="anchored-popovers-for-tests"> </div>
        <div id="virtual-popovers-for-tests">
          <popover>
            <div popover="manual"
                 tabindex="-1"
                 data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
                 id="bonsai_path_replaced_in_test"
                 class="floating_hash_replaced_in_test popover_hash_replaced_in_test"
                 custom-css-vars=((--ppx_css_update_position_anon_variable_1_hash_replaced_in_test"var(--floatingHeight, fit-content)")(--ppx_css_update_position_anon_variable_2_hash_replaced_in_test"var(--floatingWidth, fit-content)")(--ppx_css_update_position_anon_variable_3_hash_replaced_in_test"var(--floatingMinHeight)")(--ppx_css_update_position_anon_variable_4_hash_replaced_in_test"var(--floatingMinWidth)")(--ppx_css_update_position_anon_variable_5_hash_replaced_in_test"var(--floatingAvailableWidth, 100.00%)"))
                 @on_click_global
                 @on_keydown_global
                 @on_mousedown_global
                 vdom_toplayer_popover_inertness=()
                 vdom_toplayer_restore_focus_on_close=((prevent_scroll false))
                 vdom_toplayer_show_on_mount=()
                 @on_keydown>
              <div class="popover_dom__inline_class_hash_replaced_in_test">
                <div> popover </div>
              </div>
              <nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget> </nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4-widget>
            </div>
          </popover>
        </div>
        <div id="modals-for-tests"> </div>
      </html>
      |}];
    Handle.click_on ~get_vdom:Fn.id handle ~selector:"#close";
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      <html id="html-for-tests">
        <div id="app-root-for-tests">
          <div>
            <button id="open" @on_click> Open </button>
            <button id="close" @on_click> Close </button>
          </div>
        </div>
        <div id="anchored-popovers-for-tests"> </div>
        <div id="virtual-popovers-for-tests"> </div>
        <div id="modals-for-tests"> </div>
      </html>
      |}]
  ;;
end

(* Test that virtual and anchored are consistent, and that inputs are propogated to the
   hooks correctly. *)
module _ = struct
  module Result_spec = struct
    type t = Test_result.t
    type incoming = Actions.t

    let view
      { Test_result.result = { Helper.Result.anchored_popovers; virtual_popovers; _ }; _ }
      =
      match anchored_popovers, virtual_popovers with
      | [], _ :: _ -> failwith "Virtual popover is open, but anchored is not."
      | _ :: _, [] -> failwith "Anchored popover is open, but virtual is not."
      | _ :: _ :: _, _ ->
        failwith "More than one anchored popover, but expected only one."
      | _, _ :: _ :: _ -> failwith "More than one virtual popover, but expected only one."
      | [], [] -> [%sexp ([ Closed ] : Popover_test_result.t list)] |> Sexp.to_string_hum
      | [ anchored ], [ virtual_ ] ->
        let anchored_sexp =
          [%sexp
            ([ Popover_test_result.conv (Helper.Anchored_popover.inputs anchored) ]
             : Popover_test_result.t list)]
        in
        let virtual_sexp =
          [%sexp
            ([ Popover_test_result.conv virtual_.inputs ] : Popover_test_result.t list)]
        in
        (match Sexp.equal anchored_sexp virtual_sexp with
         | true -> anchored_sexp |> Sexp.to_string_hum
         | false ->
           "Error: popovers produced different result: \n"
           ^ Expect_test_patdiff.patdiff_s anchored_sexp virtual_sexp)
    ;;

    let incoming { Test_result.open_; close; reset_models; _ } = function
      | Actions.Open -> open_
      | Close -> close
      | Reset_models -> reset_models
    ;;
  end

  let get_vdom
    ~for_
    { Test_result.result = { anchored_popovers; virtual_popovers; _ }; _ }
    =
    match for_ with
    | `Virtual ->
      virtual_popovers
      |> List.hd
      |> Option.value_map
           ~f:(fun { popover_vdom; _ } -> popover_vdom)
           ~default:Vdom.Node.None
    | `Anchored ->
      anchored_popovers
      |> List.hd
      |> Option.value_map ~f:Helper.Anchored_popover.content_vdom ~default:Vdom.Node.None
  ;;

  let%expect_test "External open and closing" =
    let test_component =
      test_component ~content:(fun ~close:_ _ ->
        Bonsai.return (View.text "Popover content!"))
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}]
  ;;

  let%expect_test "Popover changing directions and alignments" =
    let position_var = Var.create Position.Top in
    let alignment_var = Var.create Alignment.Center in
    let offset_var = Var.create Offset.zero in
    let match_anchor_side_length_var = Var.create None in
    let test_component =
      test_component
        ~position:(Var.value position_var)
        ~alignment:(Var.value alignment_var)
        ~offset:(Var.value offset_var)
        ~match_anchor_side_length:(Var.value match_anchor_side_length_var)
        ~content:(fun ~close:_ _ -> Bonsai.return (View.text "Popover content!"))
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open (content_html "<span> Popover content! </span>") (arrow_html ())
        (positioning
         (((position Top) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set position_var Left;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (positioning
      -|   (((position Top) (alignment Center)
      +|   (((position Left) (alignment Center)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set position_var Top;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (positioning
      -|   (((position Left) (alignment Center)
      +|   (((position Top) (alignment Center)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set position_var Bottom;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (positioning
      -|   (((position Top) (alignment Center)
      +|   (((position Bottom) (alignment Center)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set alignment_var Start;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (positioning
      -|   (((position Bottom) (alignment Center)
      +|   (((position Bottom) (alignment Start)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set alignment_var End;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
          (positioning
      -|   (((position Bottom) (alignment Start)
      +|   (((position Bottom) (alignment End)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Var.set offset_var { main_axis = 3.; cross_axis = 2. };
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
           (((position Bottom) (alignment End)
      -|     (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      +|     (offset ((main_axis 3) (cross_axis 2))) (match_anchor_side_length ()))))))
      |}];
    Var.set offset_var { main_axis = -3.; cross_axis = 16. };
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
           (((position Bottom) (alignment End)
      -|     (offset ((main_axis 3) (cross_axis 2))) (match_anchor_side_length ()))))))
      +|     (offset ((main_axis -3) (cross_axis 16))) (match_anchor_side_length ()))))))
      |}];
    Var.set match_anchor_side_length_var (Some Grow_to_match);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
           (((position Bottom) (alignment End)
      -|     (offset ((main_axis -3) (cross_axis 16))) (match_anchor_side_length ()))))))
      +|     (offset ((main_axis -3) (cross_axis 16)))
      +|     (match_anchor_side_length (Grow_to_match)))))))
      |}];
    Var.set match_anchor_side_length_var (Some Match_exactly);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
             (offset ((main_axis -3) (cross_axis 16)))
      -|     (match_anchor_side_length (Grow_to_match)))))))
      +|     (match_anchor_side_length (Match_exactly)))))))
      |}];
    Var.set match_anchor_side_length_var (Some Shrink_to_match);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
             (offset ((main_axis -3) (cross_axis 16)))
      -|     (match_anchor_side_length (Match_exactly)))))))
      +|     (match_anchor_side_length (Shrink_to_match)))))))
      |}];
    Var.set match_anchor_side_length_var None;
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
           (((position Bottom) (alignment End)
      -|     (offset ((main_axis -3) (cross_axis 16)))
      -|     (match_anchor_side_length (Shrink_to_match)))))))
      +|     (offset ((main_axis -3) (cross_axis 16))) (match_anchor_side_length ()))))))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show_diff ~diff_context:1 handle;
    [%expect
      {|
      -|((Open (content_html "<span> Popover content! </span>") (arrow_html ())
      -|  (positioning
      -|   (((position Bottom) (alignment End)
      -|     (offset ((main_axis -3) (cross_axis 16))) (match_anchor_side_length ()))))))
      +|(Closed)
      |}]
  ;;

  let%expect_test "Closing via the internal button on click" =
    let test_component =
      test_component ~content:(fun ~close _ ->
        let%arr close in
        View.vbox
          [ View.text "Popover!"
          ; Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.on_click (fun _ -> close); Vdom.Attr.class_ "close_button" ]
              [ View.text "Close" ]
          ])
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div style={ display: flex; flex-direction: column; }>\
         \n  <span> Popover! </span>\
         \n  <button class=\"close_button\" @on_click>\
         \n    <span> Close </span>\
         \n  </button>\
         \n</div>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.click_on handle ~selector:".close_button" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".close_button" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}]
  ;;

  let%expect_test "Closing doesn't affect internal state; resetting closes the popover \
                   and clears its contents."
    =
    let test_component =
      test_component ~content:(fun ~close:_ (local_ graph) ->
        let count, set_count = Bonsai.state 0 graph in
        let%arr count and set_count in
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.on_click (fun _ -> set_count (count + 1))
            ; Vdom.Attr.class_ "increment"
            ]
          [ View.text (string_of_int count) ])
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 0 </span>\
         \n</button>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 1 </span>\
         \n</button>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 1 </span>\
         \n</button>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.do_actions handle [ Reset_models ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<button class=\"increment\" @on_click>\
         \n  <span> 0 </span>\
         \n</button>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.do_actions handle [ Close ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}]
  ;;

  let%expect_test "Resetting the insides of a popover doesn't close it" =
    let test_component =
      test_component ~content:(fun ~close:_ (local_ graph) ->
        Bonsai.with_model_resetter'
          ~f:(fun ~reset (local_ graph) ->
            let count, set_count = Bonsai.state 0 graph in
            let%arr count and set_count and reset in
            Vdom.Node.div
              [ Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.on_click (fun _ -> set_count (count + 1))
                    ; Vdom.Attr.class_ "increment"
                    ]
                  [ View.text (string_of_int count) ]
              ; Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.on_click (fun _ -> reset)
                    ; Vdom.Attr.class_ "reset_contents"
                    ]
                  [ Vdom.Node.text "reset" ]
              ])
          graph)
    in
    let handle = Handle.create (module Result_spec) test_component in
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect {| (Closed) |}];
    Handle.do_actions handle [ Open ];
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div>\
         \n  <button class=\"increment\" @on_click>\
         \n    <span> 0 </span>\
         \n  </button>\
         \n  <button class=\"reset_contents\" @on_click> reset </button>\
         \n</div>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".increment" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show handle;
    [%expect
      {|
      ((Open
        (content_html
          "<div>\
         \n  <button class=\"increment\" @on_click>\
         \n    <span> 1 </span>\
         \n  </button>\
         \n  <button class=\"reset_contents\" @on_click> reset </button>\
         \n</div>")
        (arrow_html ())
        (positioning
         (((position Auto) (alignment Center)
           (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}];
    Handle.click_on
      handle
      ~selector:".reset_contents"
      ~get_vdom:(get_vdom ~for_:`Anchored);
    Handle.click_on handle ~selector:".reset_contents" ~get_vdom:(get_vdom ~for_:`Virtual);
    Handle.recompute_view_until_stable handle;
    Handle.show_diff handle;
    [%expect
      {|
        ((Open
          (content_html
            "<div>\
           \n  <button class=\"increment\" @on_click>\
      -|   \n    <span> 1 </span>\
      +|   \n    <span> 0 </span>\
           \n  </button>\
           \n  <button class=\"reset_contents\" @on_click> reset </button>\
           \n</div>")
          (arrow_html ())
          (positioning
           (((position Auto) (alignment Center)
             (offset ((main_axis 0) (cross_axis 0))) (match_anchor_side_length ()))))))
      |}]
  ;;
end

module%test [@name "tooltips"] _ = struct
  let%expect_test "description" =
    let handle =
      Handle.create (Result_spec.vdom Fn.id) (fun _ ->
        return {%html|<div><p %{Tooltip.text "hello"}>Hello!</p></div>|})
    in
    Handle.show handle;
    [%expect
      {|
      <div>
        <p class="anchor_hash_replaced_in_test" vdom_tooltip=<omitted>> Hello! </p>
      </div>
      |}]
  ;;
end

include Helper
