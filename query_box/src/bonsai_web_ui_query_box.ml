open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Vdom

module Model = struct
  type 'k suggestion_list_state =
    | Closed
    | First_item
    | Focused of 'k
  [@@deriving equal, sexp, compare]

  type 'k t =
    { query : string
    ; suggestion_list_state : 'k suggestion_list_state
    ; offset : int
    }
  [@@deriving equal, sexp]
end

module Action = struct
  type 'k t =
    | Set_query of string
    | Move_next
    | Move_prev
    | Move_to of
        { key : 'k
        ; offset : int
        }
    | Move_next_with_fixed_offset
    | Move_prev_with_fixed_offset
    | Close_suggestions
    | Open_suggestions
  [@@deriving sexp]
end

module Suggestion_list_kind = struct
  type t =
    | Transient_overlay
    | Permanent_fixture
    | Expert
  [@@deriving sexp, compare, enumerate, equal]
end

module Expand_direction = struct
  type t =
    | Down
    | Up
  [@@deriving sexp, compare, enumerate, equal]
end

module On_focus = struct
  type t =
    | Focus_first_item
    | Do_nothing
  [@@deriving sexp, compare, enumerate, equal]
end

module On_hover_item = struct
  type t =
    | Do_nothing
    | Focus_hovered_item
  [@@deriving sexp, compare, enumerate, equal]
end

let select_key ~first_try ~then_try ~else_use =
  match first_try with
  | Some (key, _) -> Model.Focused key
  | None ->
    (match then_try with
     | (lazy (Some (key, _))) -> Model.Focused key
     | (lazy None) -> else_use)
;;

type 'k t =
  { focused_item : 'k option
  ; view : Vdom.Node.t
  ; query : string
  ; set_query : ?close_list:bool -> string -> unit Effect.t
  ; focus_input : unit Effect.t
  ; activate_for_benchmarking : unit Effect.t
  }
[@@deriving fields ~getters]

let create
  (type k cmp)
  (module Key : Comparator.S with type t = k and type comparator_witness = cmp)
  ?(initial_query = "")
  ?(max_visible_items = Bonsai.return 10)
  ?(suggestion_list_kind = Bonsai.return Suggestion_list_kind.Transient_overlay)
  ?(expand_direction = Bonsai.return Expand_direction.Down)
  ?(on_focus = Bonsai.return On_focus.Focus_first_item)
  ?(on_hover_item = Bonsai.return On_hover_item.Do_nothing)
  ?(focused_item_attr = Bonsai.return Attr.empty)
  ?(extra_list_container_attr = Bonsai.return Attr.empty)
  ?(extra_input_attr = Bonsai.return Attr.empty)
  ?(extra_attr = Bonsai.return Attr.empty)
  ?(on_blur = Bonsai.return (Effect.return ()))
  ?(modify_input_on_blur = Bonsai.return None)
  ?(modify_input_on_select = Bonsai.return (fun _focused_key _query -> ""))
  ?(ignore_tab_key = false)
  ~f
  ~on_select
  ()
  (local_ graph)
  =
  let suggestion_list_is_initialized, initialize_suggestion_list =
    Bonsai.state false graph
  in
  let inject_initialize_suggestion_list =
    let%arr initialize_suggestion_list in
    initialize_suggestion_list true
  in
  let%sub { Model.query; suggestion_list_state; offset }, inject, items, _ =
    let module Key = struct
      include Key

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let module M = struct
      type t = Key.t Model.t [@@deriving sexp_of]

      let equal a b =
        Model.equal (fun a b -> (Comparator.compare Key.comparator) a b = 0) a b
      ;;
    end
    in
    Bonsai.wrap
      graph
      ~sexp_of_model:[%sexp_of: M.t]
      ~equal:[%equal: M.t]
      ~default_model:
        { Model.query = initial_query; suggestion_list_state = Closed; offset = 0 }
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) result model action ->
        match result with
        | Inactive ->
          eprint_s
            [%message
              "An action sent to a [wrap] has been dropped because its input was not \
               present. This happens when the [wrap] is inactive when it receives a \
               message."
                [%here]];
          model
        | Active (_, _, items, max_visible_items) ->
          let suggestion_list_state =
            (* We normalize which item is focused in case the list has changed since the
               last action. Normalization just means setting the focused key to the
               closest thing that actually exists. *)
            match model.suggestion_list_state with
            | Focused key ->
              select_key
                ~first_try:(Map.closest_key items `Less_or_equal_to key)
                ~then_try:(lazy (Map.closest_key items `Greater_or_equal_to key))
                ~else_use:First_item
            | First_item -> First_item
            | Closed -> Closed
          in
          let next_suggestion_list_state () =
            match suggestion_list_state with
            | Focused key ->
              select_key
                ~first_try:(Map.closest_key items `Greater_than key)
                ~then_try:(lazy (Map.min_elt items))
                ~else_use:(Focused key)
            | First_item ->
              (match Map.min_elt items with
               | None -> First_item
               | Some (first_key, _) ->
                 (match Map.closest_key items `Greater_than first_key with
                  | None -> Focused first_key
                  | Some (second_key, _) -> Focused second_key))
            | Closed -> First_item
          in
          let prev_suggestion_list_state () =
            match model.suggestion_list_state with
            | Focused key ->
              select_key
                ~first_try:(Map.closest_key items `Less_than key)
                ~then_try:(lazy (Map.max_elt items))
                ~else_use:(Focused key)
            | First_item | Closed ->
              (match Map.max_elt items with
               | None -> First_item
               | Some (last_key, _) -> Focused last_key)
          in
          (match action with
           | Action.Set_query query ->
             let suggestion_list_state =
               match suggestion_list_state with
               | Focused key -> Model.Focused key
               | First_item | Closed -> First_item
             in
             let offset = model.offset in
             { Model.query; suggestion_list_state; offset }
           | Open_suggestions -> { model with suggestion_list_state = First_item }
           | Close_suggestions -> { model with suggestion_list_state = Closed }
           | Move_next ->
             let suggestion_list_state = next_suggestion_list_state () in
             let offset =
               let comparison =
                 Model.compare_suggestion_list_state
                   (Comparator.compare (Map.comparator items))
                   model.suggestion_list_state
                   suggestion_list_state
               in
               if comparison = 0
               then model.offset
               else if comparison < 0
               then min (max_visible_items - 1) (model.offset + 1)
               else 0
             in
             { model with suggestion_list_state; offset }
           | Move_prev ->
             let suggestion_list_state = prev_suggestion_list_state () in
             let offset =
               let comparison =
                 Model.compare_suggestion_list_state
                   (Comparator.compare (Map.comparator items))
                   model.suggestion_list_state
                   suggestion_list_state
               in
               if comparison = 0
               then model.offset
               else if comparison < 0
               then max_visible_items - 1
               else max 0 (model.offset - 1)
             in
             { model with suggestion_list_state; offset }
           | Move_to { key; offset } ->
             if Map.mem items key
             then { model with suggestion_list_state = Focused key; offset }
             else model
           | Move_next_with_fixed_offset ->
             { model with suggestion_list_state = next_suggestion_list_state () }
           | Move_prev_with_fixed_offset ->
             { model with suggestion_list_state = prev_suggestion_list_state () }))
      ~f:(fun model inject (local_ graph) ->
        let%sub { Model.query; _ } = model in
        let items =
          if%sub suggestion_list_is_initialized
          then f query graph
          else Bonsai.return (Map.empty (module Key))
        in
        let%arr model and inject and items and max_visible_items in
        model, inject, items, max_visible_items)
  in
  let focused_key =
    match%sub suggestion_list_state with
    | Focused key ->
      let%arr key and items in
      (match Map.closest_key items `Less_or_equal_to key with
       | Some (key, _) -> Some key
       | None ->
         (match Map.closest_key items `Greater_or_equal_to key with
          | Some (key, _) -> Some key
          | None -> None))
    | First_item ->
      let%arr items in
      (match Map.min_elt items with
       | Some (key, _) -> Some key
       | None -> None)
    | Closed -> Bonsai.return None
  in
  let items =
    let%arr items and max_visible_items and focused_key and offset in
    match focused_key with
    | Some focused_key ->
      let length = ref 0 in
      let items = ref items in
      let result = ref (Map.empty (module Key)) in
      (* We alternate between taking something larger and smaller than the focused key
         until we have taken [max_visible_items] or have exhausted the source list. This
         is probably not done in the most efficient manner, but it's O(max_visible_items *
         log(number_of_items)), which is probably acceptable if [max_visible_items] is
         small. *)
      let visible_items = min max_visible_items (Map.length !items) in
      let offset = min offset visible_items in
      let add_element_from_side side =
        match Map.closest_key !items side focused_key with
        | Some (key, data) ->
          result := Map.set !result ~key ~data;
          items := Map.remove !items key;
          incr length;
          true
        | None -> false
      in
      while !length < visible_items do
        (* An invariant of this loop is that we will always increment [length] every
           iteration, thus guaranteeing the termination of the loop. This is guaranteed
           because [visible_items] is defined to be at most the length of the map of items
           being pulled from, which ensures that we won't run out of items collect until
           we have met the desired length. *)
        if !length <= offset
        then (
          if not (add_element_from_side `Less_or_equal_to)
          then ignore (add_element_from_side `Greater_or_equal_to : bool))
        else if not (add_element_from_side `Greater_or_equal_to)
        then ignore (add_element_from_side `Less_or_equal_to : bool)
      done;
      !result
    | None ->
      Map.subrange
        items
        ~lower_bound:Unbounded
        ~upper_bound:
          (match Map.nth items max_visible_items with
           | Some (k, _) -> Excl k
           | None -> Unbounded)
  in
  let get_items = Bonsai.peek items graph in
  let items =
    Bonsai.assoc
      (module Key)
      items
      ~f:(fun key item (local_ _graph) ->
        let%arr key
        and item
        and get_items
        and focused_key
        and focused_item_attr
        and inject
        and on_select
        and query
        and modify_input_on_select
        and on_hover_item in
        let focused_attr =
          match focused_key with
          | Some focused_key when (Comparator.compare Key.comparator) key focused_key = 0
            -> focused_item_attr
          | _ -> Attr.empty
        in
        let move_to_effect =
          let%bind.Effect items =
            match%bind.Effect get_items with
            | Active items -> Effect.return items
            | Inactive -> Effect.never
          in
          let%bind.Effect offset = Effect.of_sync_fun (Map.rank items) key in
          let offset = Option.value offset ~default:0 in
          inject (Move_to { key; offset })
        in
        let on_mouseenter =
          match on_hover_item with
          | On_hover_item.Focus_hovered_item ->
            Attr.on_mouseenter (fun _ -> move_to_effect)
          | Do_nothing -> Vdom.Attr.empty
        in
        let attr =
          Attr.many
            [ focused_attr
            ; on_mouseenter
            ; Attr.on_click (fun _ ->
                Effect.Many
                  [ on_select key
                  ; inject (Set_query (modify_input_on_select key query))
                  ; inject Close_suggestions
                  ])
            ]
        in
        Node.div ~attrs:[ attr ] [ item ])
      graph
  in
  let%sub { attr = focus_attr; focus = focus_input; blur = blur_input } =
    Bonsai_web.Effect.Focus.on_effect ~name_for_testing:"query-box" () graph
  in
  let handle_keydown =
    let%arr inject
    and focused_key_potentially_stale = focused_key
    and focused_key = Bonsai.peek focused_key graph
    and on_select
    and expand_direction
    and suggestion_list_state
    and blur_input
    and query
    and modify_input_on_select in
    let open Vdom in
    let open Js_of_ocaml in
    fun ev ->
      let with_prevent_default effect = Effect.Many [ effect; Effect.Prevent_default ] in
      let move_next = with_prevent_default (inject Move_next) in
      let move_prev = with_prevent_default (inject Move_prev) in
      let up, down =
        match expand_direction with
        | Up -> move_next, move_prev
        | Down -> move_prev, move_next
      in
      match Dom_html.Keyboard_code.of_event ev with
      | ArrowUp -> up
      | Tab when Js.to_bool ev##.shiftKey && not ignore_tab_key ->
        (match focused_key_potentially_stale with
         | Some _ -> up
         | None -> Effect.Ignore)
      | ArrowDown -> down
      | Tab when not ignore_tab_key ->
        (match focused_key_potentially_stale with
         | Some _ -> down
         | None -> Effect.Ignore)
      | Escape ->
        (match suggestion_list_state with
         | Closed -> blur_input
         | First_item | Focused _ -> inject Action.Close_suggestions)
      | Enter ->
        (* The text in the input field might have changed between the last stabilize and
           this keydown event, so we need to force a stabilize to ensure we select the
           correct key. *)
        (match focused_key_potentially_stale with
         (* NOTE: We match on the stale value so that we have the chance of being able to
            prevent default. *)
         | None -> inject Open_suggestions
         | Some _ ->
           (match%bind.Effect focused_key with
            | Active (Some key) ->
              Effect.Many
                [ on_select key
                ; inject (Set_query (modify_input_on_select key query))
                ; inject Close_suggestions
                ]
            | Active None -> inject Open_suggestions
            | Inactive ->
              eprint_s
                [%message
                  "Potential BUG: bonsai_web_ui_query_box is ignoring Enter keystroke \
                   due to inactive focused_key"
                    [%here]];
              Effect.Ignore)
           |> with_prevent_default)
      | _ -> Effect.Ignore
  in
  let on_blur =
    let modify_on_blur =
      let peek_query = Bonsai.peek query graph in
      match%sub modify_input_on_blur with
      | None -> Bonsai.return Effect.Ignore
      | Some f ->
        let%arr peek_query and inject and f in
        (match%bind.Effect peek_query with
         | Bonsai.Computation_status.Inactive -> Effect.Ignore
         | Active query ->
           let%bind.Effect new_query = f query in
           Effect.all_unit [ inject (Set_query new_query); inject Close_suggestions ])
    in
    let%arr modify_on_blur and on_blur in
    Effect.all_unit [ modify_on_blur; on_blur ]
  in
  let suggestion_container_id = Bonsai.path_id graph in
  let input_id = Bonsai.path_id graph in
  let%arr query
  and focused_key
  and inject
  and handle_keydown
  and suggestion_list_kind
  and expand_direction
  and on_focus
  and items
  and extra_list_container_attr
  and extra_input_attr
  and extra_attr
  and suggestion_container_id
  and input_id
  and focus_attr
  and focus_input
  and inject_initialize_suggestion_list
  and on_blur in
  let container_position, suggestions_position, is_open =
    match suggestion_list_kind with
    | Suggestion_list_kind.Transient_overlay ->
      let is_open = Option.is_some focused_key in
      ( Attr.style (Css_gen.position `Relative)
      , Attr.style (Css_gen.position `Absolute)
      , is_open )
    | Expert ->
      let is_open = Option.is_some focused_key in
      Attr.empty, Attr.empty, is_open
    | Permanent_fixture -> Attr.empty, Attr.empty, true
  in
  let on_blur =
    Attr.on_blur
      (let open Js_of_ocaml in
       fun (ev : Dom_html.focusEvent Js.t) ->
         (* Blurring usually means that we want to close the suggestion list. However, if
            we are blurring the text input in order to focus the list of items (or vice
            versa), we want to keep the list open. Thus, we check whether the
            relatedTarget of the event is one of those two elements, in which case we
            don't close the list. *)
         match Js.Opt.to_option ev##.relatedTarget with
         | Some related_target ->
           let id = Js.to_string related_target##.id in
           if String.equal id suggestion_container_id || String.equal id input_id
           then Effect.Ignore
           else Effect.Many [ inject Close_suggestions; on_blur ]
         | None -> Effect.Many [ inject Close_suggestions; on_blur ])
  in
  let on_focus =
    match on_focus with
    | Do_nothing -> Vdom.Attr.empty
    | Focus_first_item ->
      Attr.on_focus (fun _ ->
        Effect.all_unit [ inject_initialize_suggestion_list; inject Open_suggestions ])
  in
  let input =
    Node.input
      ~attrs:
        [ Attr.id input_id
        ; Attr.type_ "text"
        ; Attr.string_property "value" query
        ; Attr.on_keydown (fun ev ->
            Effect.all_unit [ inject_initialize_suggestion_list; handle_keydown ev ])
        ; Attr.on_input (fun _ query ->
            (* It may seem weird to set the query first and then initialize the suggestion
               list, but injecting the suggestion list will call the user's provided
               [f:(query -> options)] function. Thus, if we initialize the suggestions
               first, we'll call [f ""] and then [f query]. This ordering ensures we only
               call [f query]. *)
            Effect.all_unit
              [ inject (Set_query query); inject_initialize_suggestion_list ])
        ; focus_attr
        ; on_focus
        ; on_blur
        ; extra_input_attr
        ]
      ()
  in
  let suggestions =
    match is_open with
    | false -> Node.div []
    | true ->
      let position_above_or_below, directed_items =
        match expand_direction with
        | Up -> Attr.style (Css_gen.bottom (`Px 0)), items |> Map.data |> List.rev
        | Down -> Attr.empty, Map.data items
      in
      let attr =
        Attr.(suggestions_position @ position_above_or_below @ extra_list_container_attr)
      in
      Node.div ~attrs:[ attr ] directed_items
  in
  let suggestions_container =
    Node.div
      ~attrs:
        [ Attr.create "data-test" "query-box-item-container"
        ; Attr.id suggestion_container_id
        ; Attr.tabindex (-1)
        ; Attr.on_wheel
            (let open Js_of_ocaml in
             fun (ev : Dom_html.wheelEvent Js.t) ->
               let comparison =
                 match expand_direction with
                 | Down -> Float.( < ) (Js.to_float ev##.deltaY) 0.0
                 | Up -> Float.( > ) (Js.to_float ev##.deltaY) 0.0
               in
               Effect.Many
                 [ (if comparison
                    then inject Move_prev_with_fixed_offset
                    else inject Move_next_with_fixed_offset)
                 ; Effect.Prevent_default
                 ])
        ; on_blur
        ; container_position
        ]
      [ suggestions ]
  in
  let view =
    Node.div
      ~attrs:[ extra_attr ]
      (match expand_direction with
       | Up -> [ suggestions_container; input ]
       | Down -> [ input; suggestions_container ])
  in
  { focused_item = focused_key
  ; view
  ; query
  ; set_query =
      (fun ?(close_list = false) query ->
        Effect.Many
          [ inject (Set_query query)
          ; (if close_list then inject Close_suggestions else Effect.Ignore)
          ])
  ; focus_input
  ; activate_for_benchmarking = inject_initialize_suggestion_list
  }
;;

module Collate_map_with_score = struct
  module Scored_key = struct
    module T = struct
      type 'k t = int * 'k

      let sexp_of_t sexp_of_k = [%sexp_of: int * k]

      let compare compare_k =
        Comparable.lexicographic
          [ Comparable.lift [%compare: int] ~f:fst; Comparable.lift compare_k ~f:snd ]
      ;;
    end

    include T
    include Comparator.Derived (T)

    module M (T : Comparator.S) = struct
      type nonrec t = T.t t
      type nonrec comparator_witness = T.comparator_witness comparator_witness

      let sexp_of_t = sexp_of_t (Comparator.sexp_of_t T.comparator)
      let comparator = comparator T.comparator
    end

    module Map = struct
      type nonrec ('k, 'v, 'cmp) t = ('k t, 'v, 'cmp comparator_witness) Map.t
    end
  end

  let collate
    (type k cmp)
    (module Cmp : Comparator.S with type t = k and type comparator_witness = cmp)
    ~preprocess
    ~score
    ~query_is_as_strict
    ~to_result
    input
    query
    =
    let empty_result = Map.empty (module Scored_key.M (Cmp)) in
    Bonsai.Incr.compute (Bonsai.both input query) ~f:(fun input_and_query ->
      let%pattern_bind.Ui_incr input, query = input_and_query in
      let%bind.Ui_incr input in
      let len = Map.length input in
      let array = Uniform_array.unsafe_create_uninitialized ~len in
      let () =
        let index = ref 0 in
        Map.iteri input ~f:(fun ~key ~data ->
          Uniform_array.set array !index (key, data, preprocess ~key ~data);
          incr index)
      in
      (* We keep track of an arbitrary number of queries. Each time the query changes, we
         discard any queries for which the new query is not merely a refinement of. In
         other words, we maintain the invariant that each item in this list of queries is
         strictly more general than the previous one. *)
      let previous_queries = ref [] in
      (* In addition, we also keep track of the index (from the back of the list of
         queries, rather than the front, but this doesn't matter because we never use the
         index to get an element out of the list) of the first query that eliminated an
         item from the set of result. *)
      let filtered_out_at_index = Array.create ~len Int.max_value in
      let%map.Ui_incr query in
      let rec trim_queries qs =
        match qs with
        | [] -> []
        | q :: qs -> if query_is_as_strict query ~as_:q then q :: qs else trim_queries qs
      in
      previous_queries := query :: trim_queries !previous_queries;
      let num_queries = List.length !previous_queries in
      Uniform_array.foldi
        array
        ~init:empty_result
        ~f:(fun index acc (key, data, preprocessed) ->
          let score =
            (* If the item was already filtered out by a previous query, we can keep
               filtering it out. If instead it was filtered out by a query that have since
               discarded (or, possibly, it was never filtered out), then we need to
               re-evaluate the score. *)
            if filtered_out_at_index.(index) < num_queries
            then 0
            else (
              let score = score query preprocessed in
              filtered_out_at_index.(index)
              <- (if score = 0 then num_queries else Int.max_value);
              score)
          in
          if score = 0
          then acc
          else (
            (* The first component of the key compares equivalently to the pair (score,
               index), but faster, since it is only an integer. Note that the map
               comparator doesn't need to inspect the key itself, since [index] already
               captures that ordering. Thus, this whole computation remains fast even if
               the input map comparator is extremely slow. *)
            let new_key = score, key in
            Map.add_exn acc ~key:new_key ~data:(to_result preprocessed ~key ~data))))
  ;;
end

module Filter_strategy = struct
  type t =
    | Fuzzy_match
    | Fuzzy_search_and_score
  [@@deriving compare, enumerate, equal, sexp_of]
end

let stringable
  (type k cmp)
  (module Key : Comparator.S with type t = k and type comparator_witness = cmp)
  ?initial_query
  ?max_visible_items
  ?suggestion_list_kind
  ?expand_direction
  ?on_focus
  ?on_hover_item
  ?focused_item_attr
  ?extra_list_container_attr
  ?extra_input_attr
  ?extra_attr
  ?(to_view = fun _ string -> Vdom.Node.text string)
  ?modify_input_on_blur
  ?(modify_input_on_select = Bonsai.return `Reset)
  ~filter_strategy
  ~on_select
  (input : (k, string, cmp) Map.t Bonsai.t)
  (* [filter_strategy] is not a [Value.t]; it would be easy to make it one by using
     [match%sub] here, but then the model would not be shared between the two branches,
     which is potentially confusing. If make both key modules be [Scored_key], then we
     could move the branch into [f] where the filtering actually happens; this would have
     the downside of causing the [Fuzzy_match] case to pay the cost of the extra data in
     the key. Since we don't expect this parameter to be changed at runtime, it is
     probably not worth the cost to make the parameter dynamic. *)
  (local_ graph)
  =
  let modify_input_on_select ~get_key (local_ _graph) =
    let%arr modify_input_on_select and input in
    match modify_input_on_select with
    | `Reset -> fun _ _ -> ""
    | `Don't_change -> fun _ query -> query
    | `Autocomplete ->
      fun key query -> Map.find input (get_key key) |> Option.value ~default:query
  in
  match filter_strategy with
  | Filter_strategy.Fuzzy_match ->
    let modify_input_on_select = modify_input_on_select ~get_key:Fn.id graph in
    create
      (module Key)
      ?initial_query
      ?max_visible_items
      ?suggestion_list_kind
      ?expand_direction
      ?on_focus
      ?on_hover_item
      ?focused_item_attr
      ?extra_list_container_attr
      ?extra_input_attr
      ?extra_attr
      ?modify_input_on_blur
      ~modify_input_on_select
      ~on_select
      ~f:(fun query ->
        Bonsai.Incr.compute (Bonsai.both query input) ~f:(fun incr ->
          let%pattern_bind.Incr query, input = incr in
          Incr_map.filter_mapi' input ~f:(fun ~key ~data:string ->
            let%map.Incr string and query in
            if Fuzzy_match.is_match ~char_equal:Char.Caseless.equal ~pattern:query string
            then Some (to_view key string)
            else None)))
      ()
      graph
  | Fuzzy_search_and_score ->
    let on_select =
      let%arr on_select in
      fun (_, key) -> on_select key
    in
    let result =
      let modify_input_on_select = modify_input_on_select ~get_key:snd graph in
      create
        (module Collate_map_with_score.Scored_key.M (Key))
        ?initial_query
        ?max_visible_items
        ?suggestion_list_kind
        ?expand_direction
        ?on_focus
        ?on_hover_item
        ?focused_item_attr
        ?extra_list_container_attr
        ?extra_input_attr
        ?extra_attr
        ?modify_input_on_blur
        ~modify_input_on_select
        ~on_select
        ~f:(fun query (local_ graph) ->
          let query =
            let%arr query in
            query, Fuzzy_search.Query.create query
          in
          Collate_map_with_score.collate
            (module Key)
            input
            query
            ~preprocess:(fun ~key:_ ~data -> data)
            ~score:(fun (_, query) item -> Fuzzy_search.score query ~item)
            ~query_is_as_strict:(fun (q, _) ~as_:(as_, _) ->
              String.is_substring q ~substring:as_)
            ~to_result:(fun item ~key ~data:_ -> to_view key item)
            graph)
        ()
        graph
    in
    let%arr result in
    { result with focused_item = Option.map result.focused_item ~f:snd }
;;
