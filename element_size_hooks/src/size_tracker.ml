open! Core
open! Import
open! Bonsai_web
open! Js_of_ocaml

module Dimension = struct
  type t =
    { width : float
    ; height : float
    }
  [@@deriving compare, equal, sexp_of]

  let zero = { width = 0.0; height = 0.0 }

  let of_size_array size_arr =
    let open Option.Let_syntax in
    let%map box = Js.array_get size_arr 0 |> Js.Optdef.to_option in
    (* This assumes writing-mode:horizontal.
       https://developer.mozilla.org/en-US/docs/Web/API/ResizeObserverEntry/borderBoxSize *)
    { width = Js.to_float box##.inlineSize; height = Js.to_float box##.blockSize }
  ;;
end

module Dimensions = struct
  type t =
    { border_box : Dimension.t
    ; content_box : Dimension.t
    }
  [@@deriving compare, equal, sexp_of]

  let zero = { border_box = Dimension.zero; content_box = Dimension.zero }
end

module T = struct
  module State = struct
    type t =
      { mutable callback : Dimensions.t -> unit
      ; mutable observer : ResizeObserver.resizeObserver Js.t option
      ; mutable last_dimensions : Dimensions.t
      }
  end

  let observe node ~(state : State.t) =
    (* We take the whole state here so that we can mutate the callback in it and witness
       the change in the observer *)
    let on_resize_observed entries _observer =
      let new_dimensions =
        let open Option.Let_syntax in
        let%bind first_entry = Js.array_get entries 0 |> Js.Optdef.to_option in
        let%bind border_box = Dimension.of_size_array first_entry##.borderBoxSize in
        let%map content_box = Dimension.of_size_array first_entry##.contentBoxSize in
        { Dimensions.border_box; content_box }
      in
      (* According to the spec, which box we're observing changes to shouldn't impact the
         dimensions that are returned; both the border box and content box dimensions
         should always be present:

         https://www.w3.org/TR/resize-observer/#resize-observer-interface

         To avoid having to use [Option.value_exn], we'll only run the callback when both
         are present, which should be always. *)
      match new_dimensions with
      | None -> ()
      | Some new_dimensions ->
        state.last_dimensions <- new_dimensions;
        state.callback new_dimensions
    in
    ResizeObserver.observe () ~node ~f:on_resize_observed
  ;;

  open State

  module Input = struct
    type t = Dimensions.t -> unit Vdom.Effect.t [@@deriving sexp_of]

    let combine left right dimensions =
      Vdom.Effect.sequence_as_sibling (left dimensions) ~unless_stopped:(fun () ->
        right dimensions)
    ;;
  end

  let wrap_with_handle dimensions ~f =
    Vdom.Effect.Expert.handle_non_dom_event_exn (f dimensions)
  ;;

  let init input _ =
    { State.observer = None
    ; callback = wrap_with_handle ~f:input
    ; last_dimensions = Dimensions.zero
    }
  ;;

  let on_mount _ state element =
    match Am_running_how_js.am_running_how with
    | `Browser | `Browser_benchmark | `Browser_test ->
      state.observer <- Some (observe ~state element)
    | `Node | `Node_benchmark | `Node_jsdom_test | `Node_test ->
      (* Resize observers aren't supported on Node, even in JSDom. *)
      ()
  ;;

  let on_mount = `Schedule_animation_frame on_mount

  let update ~old_input ~new_input state _ =
    if phys_equal old_input new_input
    then ()
    else (
      state.callback <- wrap_with_handle ~f:new_input;
      (* if the "size change" callback function changes, we should send it what we
         currently think the size is, otherwise if the element never changes size, the
         function would never get called, so whatever is ttracking the size would always
         remain clueless... *)
      state.callback state.last_dimensions)
  ;;

  let destroy _ state _ =
    Option.iter state.observer ~f:(fun observer -> observer##disconnect)
  ;;
end

module Hook = Vdom.Attr.Hooks.Make (T)

let on_change f = Vdom.Attr.create_hook "size_tracker" (Hook.create f)

module For_testing = struct
  include Hook.For_testing
end
