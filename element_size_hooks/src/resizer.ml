open! Core
open! Import
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml

let set_cursor s =
  (* The `!important` reduces the browser's need to perform an expensive style recalc. In
     profiling, it reduces the work on pointer-up and pointer-down events by ~10x *)
  let s = [%string "%{s} !important"] in
  Dom_html.window##.document##.body##.style##.cursor := Js.string s
;;

module Pointer_event = struct
  module T = struct
    type t =
      | Down
      | Up
      | Move
    [@@deriving sexp, compare, equal]
  end

  include T
  include Comparable.Make (T)

  let to_dom_html = function
    | Down -> Dom_html.Event.pointerdown
    | Up -> Dom_html.Event.pointerup
    | Move -> Dom_html.Event.pointermove
  ;;
end

module Side = struct
  type t =
    | First
    | Second
  [@@deriving sexp_of]
end

module Direction = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving equal, sexp_of]
end

module State = struct
  type t =
    { mutable listeners : Dom.event_listener_id Pointer_event.Map.t
    ; mutable animation_id : Dom_html.animation_frame_request_id
    ; mutable pointer_axis : float option
    ; mutable last_pointer_axis : float option
    ; mutable side : Side.t
    ; mutable direction : Direction.t
    ; mutable on_drag_start : unit Effect.t option
    ; mutable on_drag : (float -> unit Effect.t) option
    ; mutable on_drag_stop : unit Effect.t option
    ; mutable freeze_size : [ `Always | `While_dragging ]
    ; mutable parent_node : Dom_html.element Js.t option
    (* we need to store this in state because by the time we call destroy
       element.parentNode would be null *)
    }
  [@@deriving fields ~getters ~setters ~iterators:create]

  let create ~direction ~side ~on_drag_start ~on_drag ~on_drag_stop ~freeze_size () =
    let animation_id = request_animation_frame (Fn.const ()) in
    Fields.create
      ~listeners:Pointer_event.Map.empty
      ~animation_id
      ~pointer_axis:None
      ~last_pointer_axis:None
      ~side
      ~direction
      ~on_drag_start
      ~on_drag
      ~on_drag_stop
      ~freeze_size
      ~parent_node:None
  ;;

  let remove_event_listener = Option.iter ~f:Dom_html.removeEventListener

  let destroy { listeners; animation_id; _ } =
    Map.iter listeners ~f:Dom_html.removeEventListener;
    cancel_animation_frame animation_id
  ;;

  let on_pointer_event state element ~f ~event =
    let f event_target pointer_event =
      (match event, state with
       | Pointer_event.Down, { on_drag_start = Some effect; _ }
       | Pointer_event.Up, { on_drag_stop = Some effect; _ } ->
         Effect.Expert.handle pointer_event effect ~on_exn:(fun exn ->
           Exn.reraise exn "Unhandled exception raised in effect")
       | _ -> ());
      f event_target pointer_event
    in
    remove_event_listener (Map.find state.listeners event);
    let id = add_event_listener element (Pointer_event.to_dom_html event) ~f in
    state.listeners <- Map.set state.listeners ~key:event ~data:id
  ;;

  let remove_pointer_event state ~event =
    remove_event_listener (Map.find state.listeners event)
  ;;

  let cancel_schedule state = cancel_animation_frame state.animation_id

  let schedule state ~f =
    cancel_schedule state;
    state.animation_id <- request_animation_frame f
  ;;

  let set_pointer_axis state x = state.pointer_axis <- Some x
  let clear_pointer_axis state = state.pointer_axis <- None
  let clear_pointer_start state = state.last_pointer_axis <- None
end

let get_parent element =
  let%bind.Option parent = Js.Opt.to_option element##.parentNode in
  Js.Opt.to_option (Dom_html.CoerceTo.element parent)
;;

let rec do_update_height_or_width target state =
  let (_ : unit option) =
    let open Option.Let_syntax in
    let%bind pointer_axis = State.pointer_axis state in
    let%bind last_pointer_axis =
      let temp = State.last_pointer_axis state in
      State.set_last_pointer_axis state (Some pointer_axis);
      temp
    in
    let%bind target = Js.Opt.to_option target in
    let%bind parent = get_parent target in
    let parent_rect = parent##getBoundingClientRect in
    let parent_length =
      match State.direction state with
      | Horizontal -> parent_rect##.width
      | Vertical -> parent_rect##.height
    in
    let parent_length = Js.to_float parent_length in
    let%bind new_length =
      let operation =
        match state.side with
        | First -> Float.sub
        | Second -> Float.add
      in
      let diff_axis = pointer_axis -. last_pointer_axis in
      let proposed_length = operation parent_length diff_axis in
      let should_accept =
        let is_growing = Float.(proposed_length > parent_length) in
        let is_shrinking = Float.(proposed_length < parent_length) in
        match state.side with
        | First ->
          let parent_start =
            (match State.direction state with
             | Horizontal -> parent_rect##.left
             | Vertical -> parent_rect##.top)
            |> Js.to_float
          in
          let pointer_on_start = Float.(pointer_axis < parent_start) in
          let pointer_on_end = Float.(pointer_axis > parent_start) in
          (is_growing && pointer_on_start) || (is_shrinking && pointer_on_end)
        | Second ->
          let parent_end =
            (match State.direction state with
             | Horizontal -> parent_rect##.right
             | Vertical -> parent_rect##.bottom)
            |> Js.to_float
          in
          let pointer_on_start = Float.(pointer_axis < parent_end) in
          let pointer_on_end = Float.(pointer_axis > parent_end) in
          (is_growing && pointer_on_end) || (is_shrinking && pointer_on_start)
      in
      Option.some_if should_accept proposed_length
    in
    (match State.direction state with
     | Horizontal -> set_width parent new_length
     | Vertical -> set_height parent new_length);
    Option.iter state.on_drag ~f:(fun on_drag ->
      let effect = on_drag new_length in
      Effect.Expert.handle_non_dom_event_exn effect);
    return ()
  in
  State.clear_pointer_axis state;
  State.schedule state ~f:(fun _ -> do_update_height_or_width target state)
;;

module T = struct
  module Input = struct
    type t =
      { direction : Direction.t
      ; side : Side.t
      ; on_drag_start : (unit Effect.t option[@sexp.opaque])
      ; on_drag : ((float -> unit Effect.t) option[@sexp.opaque])
      ; on_drag_stop : (unit Effect.t option[@sexp.opaque])
      ; freeze_size : [ `Always | `While_dragging ]
      }
    [@@deriving sexp_of]

    (* Randomly pick the first, since it makes no sense to include two resizer hooks on
       the same node *)
    let combine first _second = first
  end

  module State = State

  module Helpers = struct
    (* This has to take parent_node instead of element since if we run this from destroy
       we can no longer look it up *)
    let clear_size_attr direction parent_node =
      let f =
        match direction with
        | Direction.Horizontal -> Freeze.Expert.reset_width
        | Vertical -> Freeze.Expert.reset_height
      in
      f parent_node
    ;;

    let init_or_reset_state
      direction
      (state : State.t)
      element
      ~on_drag_start
      ~on_drag
      ~on_drag_stop
      ~freeze_size
      =
      state.direction <- direction;
      state.on_drag_start <- on_drag_start;
      state.on_drag <- on_drag;
      state.on_drag_stop <- on_drag_stop;
      state.freeze_size <- freeze_size;
      let on_pointer_move _ event =
        let event : Js_of_ocaml.Dom_html.pointerEvent Js.t =
          Js_of_ocaml.Js.Unsafe.coerce event
        in
        State.set_pointer_axis
          state
          ((match direction with
            | Horizontal -> event##.clientX
            | Vertical -> event##.clientY)
           |> Js.to_float
           |> Int.of_float
           |> Int.to_float)
      in
      let on_pointer_up _ _ =
        set_cursor "initial";
        (match state.freeze_size with
         | `While_dragging -> Option.iter state.parent_node ~f:(clear_size_attr direction)
         | `Always -> ());
        State.clear_pointer_start state;
        State.clear_pointer_axis state;
        State.remove_pointer_event state ~event:Move;
        State.remove_pointer_event state ~event:Up;
        State.cancel_schedule state
      in
      let on_pointer_down _ event =
        let event : Js_of_ocaml.Dom_html.pointerEvent Js.t =
          Js_of_ocaml.Js.Unsafe.coerce event
        in
        (* We use currentTarget to ensure it is the node we attached the event listener to
           instead of a child node *)
        let target = event##.currentTarget in
        let clientAxis =
          (match direction with
           | Horizontal -> event##.clientX
           | Vertical -> event##.clientY)
          |> Js.to_float
        in
        State.set_last_pointer_axis state (Some clientAxis);
        State.on_pointer_event ~event:Move state Dom_html.document ~f:on_pointer_move;
        State.on_pointer_event ~event:Up state Dom_html.document ~f:on_pointer_up;
        State.schedule state ~f:(fun _ -> do_update_height_or_width target state);
        (match direction with
         | Horizontal -> "col-resize"
         | Vertical -> "row-resize")
        |> set_cursor
      in
      State.on_pointer_event state element ~f:on_pointer_down ~event:Down
    ;;
  end

  let init
    { Input.direction; side; on_drag_start; on_drag; on_drag_stop; freeze_size }
    element
    =
    let state =
      State.create ~direction ~side ~on_drag_start ~on_drag ~on_drag_stop ~freeze_size ()
    in
    Helpers.init_or_reset_state
      direction
      state
      element
      ~on_drag_start
      ~on_drag
      ~on_drag_stop
      ~freeze_size;
    state
  ;;

  let on_mount _init (state : State.t) element =
    state.parent_node <- get_parent element;
    match state.freeze_size with
    | `Always ->
      Option.iter
        state.parent_node
        ~f:
          (match State.direction state with
           | Horizontal -> Freeze.Expert.set_width
           | Vertical -> Freeze.Expert.set_height)
    | `While_dragging -> ()
  ;;

  let on_mount = `Schedule_animation_frame on_mount

  let update
    ~old_input:{ Input.direction = old_direction; _ }
    ~new_input:
      { Input.direction; side; on_drag_start; on_drag; on_drag_stop; freeze_size }
    (state : State.t)
    element
    =
    state.side <- side;
    state.on_drag_start <- on_drag_start;
    state.on_drag <- on_drag;
    state.on_drag_stop <- on_drag_stop;
    if not ([%equal: Direction.t] old_direction direction)
    then (
      Option.iter state.parent_node ~f:(Helpers.clear_size_attr old_direction);
      (* This might not work well if you try changing the direction while the user is
         holding their mouse down/dragging, but that's an edge case *)
      Helpers.init_or_reset_state
        direction
        state
        element
        ~on_drag_start
        ~on_drag
        ~on_drag_stop
        ~freeze_size)
  ;;

  let destroy _input (state : State.t) _element =
    set_cursor "initial";
    Option.iter state.parent_node ~f:(Helpers.clear_size_attr state.direction);
    State.destroy state
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let attr
  ?on_drag_start
  ?on_drag
  ?on_drag_stop
  ?(freeze_size = `Always)
  ?(direction = Direction.Horizontal)
  ~side
  ()
  =
  Vdom.Attr.create_hook
    "resizer"
    (Hook.create { direction; side; on_drag_start; on_drag; on_drag_stop; freeze_size })
;;
