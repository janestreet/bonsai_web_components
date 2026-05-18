open! Core
open! Bonsai_web
open! Js_of_ocaml
open Bonsai.Let_syntax
open Virtual_dom

module Mutable_state_tracker0 = struct
  module Id = Unique_id.Int ()

  let zero = Id.create ()

  type 's t =
    { unsafe_init : 's -> Id.t
    ; unsafe_destroy : Id.t -> unit
    ; modify : ('s -> unit) -> unit Effect.t
    ; read : 'a. ('s -> 'a) -> 'a list Effect.t
    }

  module Model = struct
    type 's t = 's Id.Map.t
  end

  module Action = struct
    type 's t =
      | Register of
          { id : Id.t
          ; state : 's
          }
      | Destroy of Id.t
      | Modify of ('s -> unit)
  end

  let component (type s) (graph @ local) =
    let module Model = struct
      include Model

      type nonrec t = s t
    end
    in
    let model : Model.t ref Bonsai.t =
      Bonsai.Expert.thunk ~f:(fun () -> ref Id.Map.empty) graph
    in
    let%arr model in
    let apply_action = function
      | Action.Register { id; state } -> model := Map.set !model ~key:id ~data:state
      | Destroy id -> model := Map.remove !model id
      | Modify f -> Map.iter !model ~f
    in
    let mutate =
      let unsafe_init state =
        let id = Id.create () in
        apply_action (Register { id; state });
        id
      in
      let unsafe_destroy id = apply_action (Destroy id) in
      let modify f = Effect.of_thunk (fun () -> apply_action (Modify f)) in
      let read r = Effect.of_thunk (fun () -> List.map (Map.data !model) ~f:r) in
      { unsafe_init; unsafe_destroy; modify; read }
    in
    model, mutate
  ;;
end

module State = struct
  type ('input, 'state) t =
    { mutable input : 'input
    ; mutable state : 'state
    ; mutable id : Mutable_state_tracker0.Id.t
    ; all_states : ('input, 'state) t Mutable_state_tracker0.Id.Map.t ref
    ; get_input : unit -> 'input
    ; other_instances : unit -> 'state list
    }
end

type ('input, 'state) reader = { f : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t }

module Widget = struct
  module type S = sig
    type element = private #Dom_html.element
    type input
    type state

    val init
      :  get_input:(unit -> input)
      -> other_instances:(unit -> state list)
      -> input
      -> state * element Js.t

    val update
      :  other_instances:(unit -> state list)
      -> prev_input:input
      -> input
      -> state
      -> element Js.t
      -> element Js.t

    val destroy
      :  other_instances:(unit -> state list)
      -> input
      -> state
      -> element Js.t
      -> unit
  end

  type ('input, 'state) t =
    { view : Vdom.Node.t
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    }

  let component
    (type input state)
    ?(vdom_for_testing = fun _ -> Vdom.Node.create "widget" [])
    (module M : S with type input = input and type state = state)
    input
    (local_ graph)
    =
    let id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"widget" sexp_of_opaque)
        graph
    in
    let mutable_state_tracker = Mutable_state_tracker0.component graph in
    let view =
      let%arr input
      and id
      and all_states, state_tracker = mutable_state_tracker in
      Vdom.Node.widget
        ~vdom_for_testing:(lazy (vdom_for_testing input))
        ~id
        ~init:(fun () ->
          let the_state : (input, state) State.t option ref = ref None in
          let get_input () =
            match !the_state with
            | None -> input
            | Some s -> s.State.input
          in
          let other_instances () =
            List.filter_map (Map.data !all_states) ~f:(fun { State.state; _ } ->
              match !the_state with
              | Some my_state when phys_equal my_state.state state -> None
              | _ -> Some state)
          in
          let state, element = M.init ~get_input ~other_instances input in
          let s =
            { State.input
            ; state
            ; id = Mutable_state_tracker0.zero
            ; get_input
            ; all_states
            ; other_instances
            }
          in
          the_state := Some s;
          let id = state_tracker.unsafe_init s in
          s.id <- id;
          s, element)
        ~update:(fun s element ->
          let { State.input = prev_input
              ; state
              ; all_states = _
              ; get_input = _
              ; id = _
              ; other_instances
              }
            =
            s
          in
          if phys_equal input prev_input
          then s, element
          else (
            s.input <- input;
            let element = M.update ~other_instances ~prev_input input state element in
            s, element))
        ~destroy:(fun s element ->
          let { State.input; state; id; get_input = _; all_states = _; other_instances } =
            s
          in
          state_tracker.unsafe_destroy id;
          M.destroy ~other_instances input state element)
        ()
    in
    let reader =
      let%arr _, { read; _ } = mutable_state_tracker in
      { f = (fun f -> read (fun s -> f s.input s.state)) }
    in
    let modify =
      let%arr _, { modify; _ } = mutable_state_tracker in
      fun f -> modify (fun s -> f s.input s.state)
    in
    let%arr view and reader and modify in
    { view; modify; read = reader.f }
  ;;
end

module Hook = struct
  module type S = sig
    type input
    type state

    val init
      :  get_input:(unit -> input)
      -> other_instances:(unit -> state list)
      -> input
      -> Dom_html.element Js.t
      -> state

    val update
      :  other_instances:(unit -> state list)
      -> prev_input:input
      -> input
      -> state
      -> Dom_html.element Js.t
      -> unit

    val destroy
      :  other_instances:(unit -> state list)
      -> input
      -> state
      -> Dom_html.element Js.t
      -> unit
  end

  type ('input, 'state) t =
    { attr : Vdom.Attr.t
    ; modify : ('input -> 'state -> unit) -> unit Effect.t
    ; read : 'a. ('input -> 'state -> 'a) -> 'a list Effect.t
    }

  let component
    (type input state)
    (module M : S with type input = input and type state = state)
    ~hook_name
    input
    (local_ graph)
    =
    let id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"hook-id" sexp_of_opaque)
        graph
    in
    let input_id =
      Bonsai.Expert.thunk
        ~f:(fun () -> Type_equal.Id.create ~name:"input-id" sexp_of_opaque)
        graph
    in
    let mutable_state_tracker = Mutable_state_tracker0.component graph in
    let attr =
      let%arr input
      and id
      and input_id
      and all_states, state_tracker = mutable_state_tracker in
      Vdom.Attr.create_hook
        hook_name
        (Vdom.Attr.Hooks.unsafe_create
         (* This function's API only allows creating this hook with one input value at a
            time. *)
           ~combine_inputs:(fun _ i -> i)
           ~id
           ~extra:(input, input_id)
           ~init:(fun input element ->
             let the_state = ref None in
             let get_input () =
               match !the_state with
               | None -> input
               | Some s -> s.State.input
             in
             let other_instances () =
               List.filter_map (Map.data !all_states) ~f:(fun { State.state; _ } ->
                 match !the_state with
                 | Some my_state when phys_equal my_state.state state -> None
                 | _ -> Some state)
             in
             let state = M.init ~get_input ~other_instances input element in
             let s =
               { State.input
               ; state
               ; id = Mutable_state_tracker0.zero
               ; get_input
               ; all_states
               ; other_instances
               }
             in
             the_state := Some s;
             let id = state_tracker.unsafe_init s in
             s.id <- id;
             input, (), s)
           ~update:(fun input (_, (), s) element ->
             let { State.input = prev_input
                 ; state
                 ; all_states = _
                 ; get_input = _
                 ; id = _
                 ; other_instances
                 }
               =
               s
             in
             if phys_equal input prev_input
             then input, (), s
             else (
               s.input <- input;
               M.update ~other_instances ~prev_input input state element;
               input, (), s))
           ~destroy:(fun (_, (), s) element ->
             let { State.input
                 ; state
                 ; id
                 ; get_input = _
                 ; all_states = _
                 ; other_instances
                 }
               =
               s
             in
             state_tracker.unsafe_destroy id;
             M.destroy ~other_instances input state element))
    in
    let funs =
      let%arr _, state_tracker = mutable_state_tracker in
      let modify f = state_tracker.modify (fun s -> f s.input s.state) in
      let reader = { f = (fun f -> state_tracker.read (fun s -> f s.input s.state)) } in
      modify, reader
    in
    let%arr attr
    and modify, reader = funs in
    { attr; modify; read = reader.f }
  ;;
end

module Dom_ref = struct
  type t =
    { attr : Vdom.Attr.t
    ; nodes : Dom_html.element Js.t list Effect.t
    }

  module T = struct
    type input = unit
    type state = Dom_html.element Js.t

    let init ~get_input:_ ~other_instances:_ () element = element
    let update ~other_instances:_ ~prev_input:() () _element _element = ()
    let destroy ~other_instances:_ () _element _element = ()
  end

  let tracker (local_ graph) =
    let c = Hook.component (module T) ~hook_name:"ref" (Bonsai.return ()) graph in
    let%arr { Hook.attr; read; modify = _ } = c in
    let nodes = read (fun () state -> state) in
    { attr; nodes }
  ;;
end

module Mutable_state_tracker = struct
  include Mutable_state_tracker0

  let component () graph =
    let%arr _, state_tracker = component graph in
    state_tracker
  ;;
end
