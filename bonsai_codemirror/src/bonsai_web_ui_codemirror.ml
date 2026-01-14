open! Core
module Low_level_vdom = Bonsai_web_ui_low_level_vdom
open! Bonsai_web
open Virtual_dom
open Js_of_ocaml
open Codemirror
open Bonsai.Let_syntax

module Editor_state = struct
  include State.Editor_state

  type t = (State.Editor_state.t[@sexp.opaque]) [@@deriving sexp]

  let equal = phys_equal
end

(* Using a ref to enable and disable logging so that other jsdom tests that have a
   codemirror instance in them do not get the logs unless they want them
*)
let should_log_in_tests = ref false

let state_text state =
  String.concat ~sep:"\n" (Text.Text.to_json (State.Editor_state.doc state))
;;

module Transaction = struct
  type t = State.Editor_state.t -> State.Transaction.t [@@deriving sexp]

  let equal = phys_equal

  let set_lines new_lines state =
    let open State in
    let open Text in
    let old_text = state_text state in
    let insert = if List.is_empty new_lines then Text.empty else Text.of_ new_lines in
    Editor_state.update
      state
      [ Transaction_spec.create
          ~changes:
            (Change_spec.single ~from:0 ~to_:(Js.string old_text)##.length ~insert ())
          ()
      ]
  ;;
end

module Editor_changer = struct
  type t =
    { send_transaction : Transaction.t -> unit
    ; modify_editor_view : (View.Editor_view.t -> unit) -> unit
    }

  let sexp_of_t = sexp_of_opaque
  let t_of_sexp _ = assert false
  let equal = phys_equal
end

module Widget_instantiation_id = Unique_id.Int ()

module Action = struct
  type t =
    | Add of Widget_instantiation_id.t * Editor_changer.t
    | Remove of Widget_instantiation_id.t
    | Reset
    | Send_transaction of Transaction.t
    | Set_state of ((unit -> Editor_state.t)[@sexp.opaque] [@equal.ignore])
    | Modify_editor_view of ((View.Editor_view.t -> unit)[@sexp.opaque] [@equal.ignore])
  [@@deriving sexp, equal]
end

module Model = struct
  type t =
    { send_transaction : Editor_changer.t Widget_instantiation_id.Map.t
    ; state : Editor_state.t
    }
  [@@deriving sexp, equal]
end

module Codemirror_widget = struct
  type element = Dom_html.element

  type input =
    { state : Editor_state.t
    ; inject : Action.t -> unit Effect.t
    ; path_and_generation : Path_and_generation.t
    }
  [@@deriving sexp]

  type state =
    { mutable id : Widget_instantiation_id.t
    ; mutable is_receiving_transactions : bool
    ; inject : (Action.t -> unit Effect.t) ref
    ; editor_view : (View.Editor_view.t[@sexp.opaque])
    }
  [@@deriving sexp]

  let destroy ~other_instances:_ (prev_input : input) (state : state) (_ : element Js.t) =
    state.is_receiving_transactions <- false;
    Effect.Expert.handle_non_dom_event_exn (prev_input.inject (Remove state.id));
    View.Editor_view.destroy state.editor_view
  ;;

  let add_send_transaction
    (input : input)
    ({ editor_view; is_receiving_transactions; id; _ } : state)
    =
    let send_transaction transaction =
      let transaction = transaction (View.Editor_view.state editor_view) in
      if is_receiving_transactions
      then View.Editor_view.dispatch editor_view transaction
      else Effect.Expert.handle_non_dom_event_exn (input.inject (Remove id))
    in
    let modify_editor_view f = f editor_view in
    Effect.Expert.handle_non_dom_event_exn
      (input.inject (Add (id, { Editor_changer.send_transaction; modify_editor_view })))
  ;;

  let init ~(get_input : unit -> input) ~other_instances (input : input) =
    let inject = ref input.inject in
    let id = Widget_instantiation_id.create () in
    let editor_view =
      View.Editor_view.create
        (View.Config.create
           ~state:input.state
           ~dispatch:
             (Js.wrap_callback (fun transaction editor_view ->
                (* Apply the transaction to the current editor *)
                View.Editor_view.update editor_view [ transaction ];
                (* Pull the newly updated state from the current editor to apply to all
                   other editors *)
                let new_state = View.Editor_view.state editor_view in
                let other_instances = other_instances () in
                List.iter other_instances ~f:(fun { editor_view; id = other_id; _ } ->
                  (match am_running_how with
                   | `Node_jsdom_test | `Browser_test ->
                     (* We don't include regular node tests, because Codemirror widgets
                        only run in JSDom. *)
                     if !should_log_in_tests
                     then
                       print_endline
                         [%string
                           "From widget id %{id#Widget_instantiation_id} updating other \
                            widget id %{other_id#Widget_instantiation_id}"]
                   | `Browser | `Browser_benchmark | `Node | `Node_benchmark | `Node_test
                     -> ());
                  View.Editor_view.set_state editor_view new_state);
                let input = get_input () in
                Effect.Expert.handle_non_dom_event_exn
                  (input.inject
                     (Set_state (fun () -> View.Editor_view.state editor_view)))))
           ())
    in
    let state = { id; is_receiving_transactions = true; editor_view; inject } in
    add_send_transaction input state;
    state, (View.Editor_view.dom editor_view : Dom_html.element Js.t)
  ;;

  let update
    ~other_instances:_
    ~prev_input:({ path_and_generation = old_path_and_generation; _ } : input)
    ({ path_and_generation = new_path_and_generation; inject; state = new_state } as input :
      input)
    ({ editor_view; _ } as widget_state : state)
    (_ : element Js.t)
    : element Js.t
    =
    widget_state.inject := inject;
    (* All updating should be taken care of directly through the editors internal updating
       functions. We only set the state here whenever we know that the model has been
       reset *)
    if Path_and_generation.distinct old_path_and_generation new_path_and_generation
    then (
      (* We have to re-add [send_transaction] to the state if we've reset the model as
         resetting will remove it.

         I tried resetting the models so that they would keep the old [send_transaction]
         but reset everything else, but for some reason that prevented the codemirror
         instance from being reset.
      *)
      add_send_transaction input widget_state;
      View.Editor_view.set_state editor_view new_state);
    (View.Editor_view.dom editor_view : element Js.t)
  ;;
end

let codemirror_widget = Low_level_vdom.Widget.component (module Codemirror_widget)

module For_testing = struct
  module Inject_hook = Vdom.Attr.No_op_hook (struct
      module Input = struct
        type t = (State.Editor_state.t -> State.Transaction.t) -> unit Effect.t
        [@@deriving sexp]

        let combine _ second = second
      end

      let name = "codemirror-test-hook"
    end)

  let type_id = Inject_hook.type_id
  let enable_logging () = should_log_in_tests := true
  let disable_logging () = should_log_in_tests := false

  let with_logging f =
    enable_logging ();
    f ();
    disable_logging ()
  ;;
end

type t =
  { view : Vdom.Node.t
  ; state : State.Editor_state.t
  ; send_transaction : (State.Editor_state.t -> State.Transaction.t) -> unit Effect.t
  ; execute_command : View.Command.t -> unit Effect.t
  ; focus : unit Effect.t
  ; blur : unit Effect.t
  }
[@@deriving fields ~getters]

let text { state; _ } = state_text state
let set_lines t new_lines = t.send_transaction (Transaction.set_lines new_lines)

let of_initial_state ?name initial_state (local_ graph) =
  let default_model =
    { Model.send_transaction = Widget_instantiation_id.Map.empty; state = initial_state }
  in
  let state, inject =
    Bonsai.actor
      graph
      ~sexp_of_model:[%sexp_of: Model.t]
      ~equal:[%equal: Model.t]
      ~sexp_of_action:[%sexp_of: Action.t]
      ~default_model
      ~recv:(fun _ctx model action ->
        let model =
          match action with
          | Add (id, editor_changer) ->
            { model with
              send_transaction =
                Map.set model.send_transaction ~key:id ~data:editor_changer
            }
          | Remove id ->
            { model with send_transaction = Map.remove model.send_transaction id }
          | Reset -> { model with send_transaction = Widget_instantiation_id.Map.empty }
          | Send_transaction transaction ->
            (match Map.min_elt model.send_transaction with
             | None ->
               (* If the map of transaction-dispatching functions is empty, that means
                  that this transaction would get ignored. Thus, we make a new editor_view
                  to apply the transaction to, and then throw away immediately afterward. *)
               let editor_state = State.Transaction.state (transaction model.state) in
               { model with state = editor_state }
             | Some (_, { Editor_changer.send_transaction; modify_editor_view = _; _ }) ->
               (* We're choosing an arbitrary element because the dispatch function set on
                  the view during creation will keep all of the states in sync so long as
                  we send the transaction to one element *)
               (try send_transaction transaction with
                | error ->
                  eprint_s
                    [%message "failed to send codemirror transaction" (error : exn)]);
               model)
          | Set_state get_state ->
            (* The only time state gets set is when we set it from within the [dispatch]
               function. The user of this library is not allowed to call [Set_state]. This
               means that this state value is only really useful when the user wants to
               maintain state between two instances of the same widget, which is doable
               through the previous state value being pased to a new instance of the
               widget the next time it is called
            *)
            let state = get_state () in
            if not (phys_equal model.state state) then { model with state } else model
          | Modify_editor_view f ->
            Map.iter
              model.send_transaction
              ~f:(fun { send_transaction = _; modify_editor_view; _ } ->
                try
                  Effect.Expert.handle_non_dom_event_exn
                    (Effect.of_sync_fun (fun () -> modify_editor_view f) ())
                with
                | error -> eprint_s [%message "failed to read editor view" (error : exn)]);
            model
        in
        model, ())
  in
  let%sub { state; _ } = state in
  let () =
    Bonsai.Edge.lifecycle
      ~on_deactivate:
        (let%map inject in
         inject Reset)
      graph
  in
  let path_and_generation = Path_and_generation.model_resetter_generation graph in
  let view =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_test | `Browser_benchmark | `Node_jsdom_test ->
      let widget_input =
        let%arr state and inject and path_and_generation in
        ({ state; inject; path_and_generation } : Codemirror_widget.input)
      in
      let codemirror = codemirror_widget widget_input graph in
      let%arr codemirror in
      codemirror.view
    | `Node | `Node_benchmark | `Node_test ->
      let send_transaction =
        let%arr inject in
        fun transaction -> inject (Send_transaction transaction)
      in
      let%arr state and send_transaction in
      Vdom.Node.create
        "codemirror"
        ~attrs:
          [ (match name with
             | Some name -> Vdom.Attr.create "data-codemirror-editor" name
             | None -> Vdom.Attr.empty)
          ; For_testing.Inject_hook.attr send_transaction
          ]
        [ Vdom.Node.text (state_text state) ]
  in
  let%arr state and view and inject in
  { view
  ; state
  ; send_transaction = (fun transaction -> inject (Send_transaction transaction))
  ; focus = inject (Modify_editor_view View.Editor_view.focus)
  ; blur =
      inject
        (Modify_editor_view
           (fun editor_view -> (View.Editor_view.content_dom editor_view)##blur))
  ; execute_command =
      (fun command ->
        inject
          (Modify_editor_view
             (fun editor_view -> (ignore : bool -> unit) (command editor_view))))
  }
;;

let with_dynamic_extensions
  ?(basic_setup = `Minimal)
  ?name
  ?sexp_of
  ~equal
  ~initial_state
  ~compute_extensions
  value
  (local_ graph)
  =
  let cm = of_initial_state ?name initial_state graph in
  let setup_extension =
    match basic_setup with
    | `Minimal -> Some Basic_setup.minimal_setup
    | `Basic -> Some Basic_setup.basic_setup
    | `None -> None
  in
  let () =
    let callback =
      let%arr cm and compute_extensions in
      fun value ->
        let open State in
        let extensions = compute_extensions value @ Option.to_list setup_extension in
        cm.send_transaction (fun state ->
          Editor_state.update
            state
            [ Transaction_spec.create
                ~effects:
                  [ State_effect_type.of_
                      State_effect.reconfigure
                      (Extension.of_list extensions)
                  ]
                ()
            ])
    in
    Bonsai.Edge.on_change
      ~trigger:`After_display
      ?sexp_of_model:sexp_of
      ~equal
      value
      ~callback
      graph
  in
  cm
;;

module Some_model = struct
  type t =
    { send_transaction : Editor_changer.t Widget_instantiation_id.Map.t
    ; state : Editor_state.t option
    }
  [@@deriving sexp, equal]
end

type wrapped_state_result =
  { view : Vdom.Node.t
  ; state : Editor_state.t
  ; inject : Action.t -> unit Effect.t
  }

(* This reduces how ugly the apply action is. It kinda only works here due to [wrap] only
   really being used to set the initial value of the state and then the model state being
   used as the source of truth. *)
let get_state_from_result_or_model result (model : Some_model.t) =
  match result with
  | Bonsai.Computation_status.Active result -> Some result.state
  | Inactive -> model.state
;;

let with_dynamic_extensions' ~name ~(initial_text : string) ~extensions (local_ graph) =
  (* In order to get the state to load initially, we have to create the state and then
     recompute the state by applying the extensions. This makes more sense than the other
     version, as that version requires extension t obe applied twice, which is a bit
     strange *)
  let default_model =
    { Some_model.send_transaction = Widget_instantiation_id.Map.empty; state = None }
  in
  let wrapped_state =
    Bonsai.wrap
      ~equal:phys_equal
      ~default_model
      ~apply_action:(fun _ctx result model (action : Action.t) ->
        let model =
          match action with
          | Add (id, editor_changer) ->
            { model with
              send_transaction =
                Map.set model.send_transaction ~key:id ~data:editor_changer
            }
          | Remove id ->
            { model with send_transaction = Map.remove model.send_transaction id }
          | Reset -> { model with send_transaction = Widget_instantiation_id.Map.empty }
          | Send_transaction transaction ->
            (match Map.min_elt model.send_transaction with
             | None ->
               (* If the map of transaction-dispatching functions is empty, that means
                  that this transaction would get ignored. Thus, we make a new editor_view
                  to apply the transaction to, and then throw away immediately afterward. *)
               (match get_state_from_result_or_model result model with
                | None -> model
                | Some state ->
                  let editor_state = State.Transaction.state (transaction state) in
                  { model with state = Some editor_state })
             | Some (_, { Editor_changer.send_transaction; modify_editor_view = _; _ }) ->
               (* We're choosing an arbitrary element because the dispatch function set on
                  the view during creation will keep all of the states in sync so long as
                  we send the transaction to one element *)
               (try send_transaction transaction with
                | error ->
                  eprint_s
                    [%message "failed to send codemirror transaction" (error : exn)]);
               model)
          | Set_state get_state ->
            (* The only time state gets set is when we set it from within the [dispatch]
               function. The user of this library is not allowed to call [Set_state]. This
               means that this state value is only really useful when the user wants to
               maintain state between two instances of the same widget, which is doable
               through the previous state value being pased to a new instance of the
               widget the next time it is called *)
            let state = get_state () in
            (match get_state_from_result_or_model result model with
             | None -> { model with state = Some state }
             | Some model_state ->
               if not (phys_equal model_state state)
               then { model with state = Some state }
               else model)
          | Modify_editor_view f ->
            Map.iter
              model.send_transaction
              ~f:(fun { send_transaction = _; modify_editor_view; _ } ->
                try
                  Effect.Expert.handle_non_dom_event_exn
                    (Effect.of_sync_fun (fun () -> modify_editor_view f) ())
                with
                | error -> eprint_s [%message "failed to read editor view" (error : exn)]);
            model
        in
        model)
      ~f:(fun model inject (local_ graph) ->
        let () =
          (* Using the inject function for some reason was causing the wrapped version to
             not actually reset *)
          Bonsai.Edge.lifecycle
            ~on_deactivate:
              (let%arr inject in
               inject Reset)
            graph
        in
        let state =
          let%arr { state; _ } = model
          and extensions in
          match state with
          | None ->
            State.Editor_state.create
              (State.Editor_state_config.create ~extensions ~doc:initial_text ())
          | Some state -> state
        in
        let path_and_generation = Path_and_generation.model_resetter_generation graph in
        let view =
          match Bonsai_web.am_running_how with
          | `Browser | `Browser_test | `Browser_benchmark | `Node_jsdom_test ->
            let widget_input =
              let%arr state and inject and path_and_generation in
              ({ state; inject; path_and_generation } : Codemirror_widget.input)
            in
            let codemirror = codemirror_widget widget_input graph in
            let%arr codemirror in
            codemirror.view
          | `Node | `Node_benchmark | `Node_test ->
            let send_transaction =
              let%arr inject in
              fun transaction -> inject (Send_transaction transaction)
            in
            let%arr state and send_transaction in
            Vdom.Node.create
              "codemirror"
              ~attrs:
                [ Vdom.Attr.create "data-codemirror-editor" name
                ; For_testing.Inject_hook.attr send_transaction
                ]
              [ Vdom.Node.text (state_text state) ]
        in
        let () =
          Bonsai.Edge.on_change
            ~trigger:`After_display
            ~equal:phys_equal
            ~callback:
              (let%arr inject in
               fun extensions ->
                 let transaction state =
                   let effects =
                     [ State.State_effect_type.of_
                         State.State_effect.reconfigure
                         (State.Extension.of_list extensions)
                     ]
                   in
                   Editor_state.update state [ State.Transaction_spec.create ~effects () ]
                 in
                 inject (Send_transaction transaction))
            extensions
            graph
        in
        let%arr state and view and inject in
        { view; state; inject })
      graph
  in
  let%arr { view; state; inject } = wrapped_state in
  { view
  ; state
  ; send_transaction = (fun transaction -> inject (Send_transaction transaction))
  ; focus = inject (Modify_editor_view View.Editor_view.focus)
  ; blur =
      inject
        (Modify_editor_view
           (fun editor_view -> (View.Editor_view.content_dom editor_view)##blur))
  ; execute_command =
      (fun command ->
        inject
          (Modify_editor_view
             (fun editor_view -> (ignore : bool -> unit) (command editor_view))))
  }
;;

module Private_for_tests = struct
  module Path_and_generation = Path_and_generation
end
