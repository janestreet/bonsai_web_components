open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Keystroke = Vdom_keyboard.Keystroke
module Keyboard_code = Keystroke.Keyboard_code
module Keyboard_code_set = Set.Make (Keystroke.Keyboard_code)

module For_testing = struct
  let platform = ref "Linux"

  let set_platform (new_platform : [ `Mac | `Linux | `Windows ]) : unit =
    let new_platform =
      (* These values would come from window.navigator.platform in browsers. *)
      match new_platform with
      | `Mac -> "MacIntel"
      | `Linux -> "Linux"
      | `Windows -> "Win32"
    in
    platform := new_platform
  ;;

  let with_platform new_platform ~f =
    let original_platform = !platform in
    set_platform new_platform;
    Exn.protect ~f ~finally:(fun () -> platform := original_platform)
  ;;
end

module Osx_behavior = struct
  type t =
    | Do_nothing
    | Treat_meta_as_ctrl
  [@@deriving sexp_of, equal]
end

module Hint_status = struct
  type t =
    | Show
    | Hide
  [@@deriving sexp_of, equal]

  let data_attr_name = "data-bonsai-web-keyboard-shortcut-hint"
  let active_selector = [%string {|[%{data_attr_name}="active"]|}]
  let inactive_selector = [%string {|[%{data_attr_name}="inactive"]|}]
end

module Hint_trigger_keys = struct
  type t =
    { ctrl : bool
    ; alt : bool
    ; shift : bool
    }
  [@@deriving sexp_of, equal]

  let default = { ctrl = true; alt = true; shift = true }

  let should_trigger t ~(key : Keyboard_code.t) =
    match key with
    | AltLeft | AltRight -> t.alt
    | ControlLeft | ControlRight -> t.ctrl
    | ShiftLeft | ShiftRight -> t.shift
    | _ -> false
  ;;
end

module Listener = struct
  type t = { effect : unit Effect.t }
end

module Model = struct
  type t =
    { active_modifiers : Keyboard_code_set.t
        (* The listener map is keyed by the [Bonsai.path_id] of the registering
           computation. *)
    ; listeners : Listener.t String.Map.t Keystroke.Map.t
    ; attr : Vdom.Attr.t
    ; is_focus_within : bool
    }

  let empty =
    { active_modifiers = Keyboard_code_set.empty
    ; listeners = Keystroke.Map.empty
    ; attr = Vdom.Attr.empty
    ; is_focus_within = false
    }
  ;;
end

module Action = struct
  type t =
    | Modifier_keydown of Keyboard_code.t
    | Modifier_keyup of Keyboard_code.t
    | Focus_in
    | Focus_out
    | Activate of
        { id : string
        ; key : Keystroke.t
        ; listener : Listener.t
        }
    | Deactivate of { id : string }
end

let dispatch key (listeners_by_key : Listener.t String.Map.t Keystroke.Map.t) =
  let listeners = Map.find listeners_by_key key in
  match listeners with
  | None -> []
  | Some listeners -> List.map (Map.data listeners) ~f:(fun listener -> listener.effect)
;;

let consume event = Js_of_ocaml.Dom_html.stopPropagation event

let is_macos () =
  let open Js_of_ocaml in
  let platform =
    if am_running_test
    then !For_testing.platform
    else (
      let platform_js = Dom_html.window##.navigator##.platform in
      Js.to_string platform_js)
  in
  (* If `navigator.platform` begins with "Mac", the OS is mac. See:
     https://developer.mozilla.org/en-US/docs/Web/API/Navigator/platform *)
  String.is_substring_at platform ~pos:0 ~substring:"Mac"
;;

let get_event_handlers ~osx_behavior ~hint_trigger_keys ~listeners ~inject ~mode =
  (* These handlers _synchronously_ determine if the event is handled by a shortcut we
     know about, and if so cancel event propagation. This is important to not trigger
     browser shortcuts and especially important to not trigger VSCode shortcuts when the
     UI is embedded. *)
  let transform_keystroke_for_osx keystroke =
    match osx_behavior with
    | Osx_behavior.Treat_meta_as_ctrl ->
      (* If both meta and ctrl are held, skip any transform as it'd be a noop. *)
      if is_macos () && Keystroke.meta keystroke && not (Keystroke.ctrl keystroke)
      then
        Keystroke.create
          ~shift:(Keystroke.shift keystroke)
          ~ctrl:true
          ~meta:false
          ~alt:(Keystroke.alt keystroke)
          (Keystroke.key keystroke)
      else keystroke
    | Osx_behavior.Do_nothing -> keystroke
  in
  let handle_event event ~kind =
    let original_keystroke = Keystroke.of_event event in
    let keystroke = transform_keystroke_for_osx original_keystroke in
    let key = Keystroke.key keystroke in
    match Hint_trigger_keys.should_trigger hint_trigger_keys ~key with
    | true ->
      let action =
        match kind with
        | `Up -> Action.Modifier_keyup key
        | `Down -> Modifier_keydown key
      in
      inject action
    | _ ->
      (match kind with
       | `Up -> Effect.Ignore
       | `Down ->
         let handlers = dispatch keystroke listeners in
         (match handlers with
          | [] -> Effect.Ignore
          | handlers ->
            consume event;
            Effect.Many handlers))
  in
  let keydown = handle_event ~kind:`Down in
  let keyup = handle_event ~kind:`Up in
  let on_keydown =
    match mode with
    | `Local -> Vdom.Attr.on_keydown keydown
    | `Global -> Vdom.Attr.Global_listeners.keydown ~phase:Bubbling ~f:keydown
  in
  let on_keyup =
    match mode with
    | `Local -> Vdom.Attr.on_keyup keyup
    | `Global -> Vdom.Attr.Global_listeners.keyup ~phase:Bubbling ~f:keyup
  in
  let on_focusin =
    match mode with
    | `Local -> Vdom.Attr.on_focusin (fun _ -> inject Focus_in)
    | `Global ->
      Vdom.Attr.Global_listeners.focusin ~phase:Capture ~f:(fun _ -> inject Focus_in)
  in
  let on_focusout =
    match mode with
    | `Local -> Vdom.Attr.on_focusout (fun _ -> inject Focus_out)
    | `Global ->
      Vdom.Attr.Global_listeners.focusout ~phase:Capture ~f:(fun _ -> inject Focus_out)
  in
  Vdom.Attr.many [ on_keydown; on_keyup; on_focusin; on_focusout ]
;;

let make_shortcut_state
  ~osx_behavior
  ~hint_trigger_keys
  ~parent_inject
  ~mode
  (local_ graph)
  =
  let default_model = Model.empty in
  let apply_action _ (model : Model.t) (action : Action.t) =
    match action with
    | Modifier_keydown modifier ->
      { model with active_modifiers = Set.add model.active_modifiers modifier }
    | Modifier_keyup modifier ->
      { model with active_modifiers = Set.remove model.active_modifiers modifier }
    | Focus_out ->
      { model with active_modifiers = Keyboard_code_set.empty; is_focus_within = false }
    | Focus_in -> { model with is_focus_within = true }
    | Activate { id; key; listener } ->
      let listeners =
        Map.update model.listeners key ~f:(function
          | Some listeners -> Map.set listeners ~key:id ~data:listener
          | None -> String.Map.singleton id listener)
      in
      { model with listeners }
    | Deactivate { id } ->
      let listeners =
        Map.filter_map model.listeners ~f:(fun listeners ->
          let listeners = Map.remove listeners id in
          Option.some_if (not (Map.is_empty listeners)) listeners)
      in
      { model with listeners }
  in
  let model, inject = Bonsai.state_machine ~default_model ~apply_action graph in
  let inject =
    let%arr parent_inject and inject in
    fun action ->
      let propagate_active_modifier_actions_to_parent =
        match action with
        | Action.Modifier_keydown _ | Modifier_keyup _ -> parent_inject action
        | Activate _ | Deactivate _ | Focus_in | Focus_out -> Effect.Ignore
      in
      Effect.Many [ inject action; propagate_active_modifier_actions_to_parent ]
  in
  let attr =
    let%arr { Model.listeners; active_modifiers; _ } = model
    and inject in
    let event_handlers =
      get_event_handlers ~osx_behavior ~hint_trigger_keys ~listeners ~inject ~mode
    in
    let hint_status_attr =
      match mode with
      | `Global ->
        (* Unclear which node we'd add a data attr to. Maybe <body>. *)
        Vdom.Attr.empty
      | `Local ->
        let value = if Set.is_empty active_modifiers then "inactive" else "active" in
        Vdom.Attr.create Hint_status.data_attr_name value
    in
    Vdom.Attr.many [ event_handlers; hint_status_attr ]
  in
  let%arr model and attr and inject in
  { model with attr }, inject
;;

module Dynamic_scope = struct
  type t = (Model.t * (Action.t -> unit Effect.t)) Bonsai.Dynamic_scope.t

  let fallback =
    let log_and_do_nothing action =
      match (action : Action.t) with
      | Activate { id; _ } ->
        if am_running_test
        then ()
        else print_s [%message "No keyboard shortcut listener installed" (id : string)];
        Effect.Ignore
      | _ -> Effect.Ignore
    in
    Model.empty, log_and_do_nothing
  ;;

  let instance : t =
    Bonsai.Dynamic_scope.create ~name:"keyboard-shortcut-state" ~fallback ()
  ;;
end

module Expert = struct
  let install_listener
    ?(osx_behavior = Osx_behavior.Treat_meta_as_ctrl)
    ?(hint_trigger_keys = Hint_trigger_keys.default)
    ~mode
    inside
    (local_ graph)
    =
    let%sub _parent_state, parent_inject =
      Bonsai.Dynamic_scope.lookup Dynamic_scope.instance graph
    in
    let state =
      make_shortcut_state ~osx_behavior ~hint_trigger_keys ~parent_inject ~mode graph
    in
    Bonsai.Dynamic_scope.set Dynamic_scope.instance state ~inside graph
  ;;

  let install_manual_listener
    ?(osx_behavior = Osx_behavior.Treat_meta_as_ctrl)
    ?(hint_trigger_keys = Hint_trigger_keys.default)
    inside
    (local_ graph)
    =
    let%sub _parent_state, parent_inject =
      Bonsai.Dynamic_scope.lookup Dynamic_scope.instance graph
    in
    (* This doesn't _need_ to always be local, but no usecases need it yet, so we keep the
       API simple for now. *)
    let state =
      make_shortcut_state
        ~osx_behavior
        ~hint_trigger_keys
        ~parent_inject
        ~mode:`Local
        graph
    in
    let dispatch_keyboard_event =
      let%arr { Model.listeners; _ }, _ = state in
      fun keystroke ->
        let handlers = dispatch keystroke listeners in
        Effect.all_unit handlers
    in
    let new_computation =
      Bonsai.Dynamic_scope.set Dynamic_scope.instance state ~inside graph
    in
    new_computation, dispatch_keyboard_event
  ;;

  let get_event_handler_for_nearest_installation (local_ graph) =
    let%sub { attr; _ }, _ = Bonsai.Dynamic_scope.lookup Dynamic_scope.instance graph in
    attr
  ;;
end

let install_listener
  ?osx_behavior
  ?hint_trigger_keys
  ~mode
  (inside : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
  (local_ graph)
  : Vdom.Node.t Bonsai.t
  =
  let%sub view, attr =
    Expert.install_listener
      ?osx_behavior
      ?hint_trigger_keys
      ~mode
      (fun (local_ graph) ->
        let view : Vdom.Node.t Bonsai.t = inside graph in
        let attr : Vdom.Attr.t Bonsai.t =
          Expert.get_event_handler_for_nearest_installation graph
        in
        Bonsai.both view attr)
      graph
  in
  let%arr view and attr in
  match (view : Vdom.Node.t) with
  | Element elem ->
    Vdom.Node.Element.map_attrs elem ~f:(Vdom.Attr.combine attr) |> Vdom.Node.Element
  | node -> Vdom.Node.div ~attrs:[ attr ] [ node ]
;;

(* Show hints if any modifier (ctrl or alt) is pressed *)
let hint_status (local_ graph) =
  let%sub { active_modifiers; _ }, _ =
    Bonsai.Dynamic_scope.lookup Dynamic_scope.instance graph
  in
  let%arr active_modifiers in
  match Set.is_empty active_modifiers with
  | true -> Hint_status.Hide
  | false -> Hint_status.Show
;;

(* [prevent_default] determines whether to call [event.preventDefault] on the browser
   event. This is useful because the registered [effect] cannot call it itself. Any
   [Effect.Prevent_default] within the registered effect is executed after the event has
   already dissapeared because we peek it for correctness, and that happens too late.
*)
let register ?(prevent_default = Bonsai.return true) ~effect keystroke (local_ graph) =
  let id = Bonsai.path_id graph in
  let%sub _, inject = Bonsai.Dynamic_scope.lookup Dynamic_scope.instance graph in
  let effect = Bonsai.peek effect graph in
  let on_activate =
    let%arr id
    and inject
    and key = keystroke
    and effect
    and prevent_default in
    let peeked_effect =
      match%bind.Effect effect with
      | Active effect -> effect
      | Inactive -> Effect.Ignore
    in
    let effect =
      Effect.Many
        [ (if prevent_default
           then Effect.Prevent_default [@alert "-deprecated"]
           else Effect.Ignore)
        ; peeked_effect
        ]
    in
    inject (Activate { id; key; listener = { effect } })
  in
  let on_deactivate =
    let%arr id and inject in
    inject (Deactivate { id })
  in
  let () =
    let callback =
      let%arr on_activate in
      Fn.const on_activate
    in
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ~equal:[%equal: Keystroke.t]
      keystroke
      ~callback
      graph
  in
  Bonsai.Edge.lifecycle ~on_activate ~on_deactivate graph
;;
