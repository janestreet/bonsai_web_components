open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Item = struct
  type ('effect, 'item) t =
    | Single of
        { key : string
        ; disabled : bool
        ; on_click : 'effect Effect.t
        ; item : 'item
        }
    | Section of
        { title : string option
        ; items : ('effect, 'item) t list
        }
    | Inert of 'item
    | Submenu of
        { key : string
        ; disabled : bool
        ; item : 'item
        ; items : ('effect, 'item) t list
        }

  let sexp_of_t _sexp_of_effect sexp_of_item t =
    let rec sexp_of_t = function
      | Single { key; disabled; on_click = _; item } ->
        [%message
          "Single" (key : string) (disabled : bool) ~item:(sexp_of_item item : Sexp.t)]
      | Section { title; items } ->
        let items = List.map items ~f:sexp_of_t in
        [%message "Section" (title : string option) (items : Sexp.t list)]
      | Inert item -> List [ Atom "Inert"; sexp_of_item item ]
      | Submenu { key; disabled; item; items } ->
        let items = List.map items ~f:sexp_of_t in
        [%message
          "Submenu"
            (key : string)
            (disabled : bool)
            ~item:(sexp_of_item item : Sexp.t)
            (items : Sexp.t list)]
    in
    sexp_of_t t
  ;;

  let rec map_actions item ~f =
    match (item : _ t) with
    | Single single -> Single { single with on_click = f single.on_click }
    | Inert item -> Inert item
    | Section section ->
      Section { section with items = List.map section.items ~f:(map_actions ~f) }
    | Submenu submenu ->
      Submenu { submenu with items = List.map submenu.items ~f:(map_actions ~f) }
  ;;

  let key = function
    | Single { key; _ } | Submenu { key; _ } -> Some key
    | Section _ | Inert _ -> None
  ;;

  let selectable_key = function
    | Single { disabled = false; key; _ }
    | Submenu { disabled = false; items = _ :: _; key; _ } -> Some key
    | Single { disabled = true; _ }
    | Submenu { disabled = true; _ }
    | Submenu { disabled = false; items = []; _ }
    | Section _ | Inert _ -> None
  ;;

  let is_selectable t = Option.is_some (selectable_key t)
  let selectable_item item = Option.some_if (is_selectable item) item

  let find_map
    (items : ('effect, 'item) t list)
    ~(f : path:string list -> ('effect, 'item) t -> 'b option)
    : 'b option
    =
    let rec loop ~path = function
      | Section { items; title = _ } -> List.find_map items ~f:(loop ~path)
      | Submenu { items; key; _ } as item ->
        Option.first_some_thunk (f ~path item) (fun () ->
          if is_selectable item
          then List.find_map items ~f:(loop ~path:(path @ [ key ]))
          else None)
      | (Single _ | Inert _) as item -> f ~path item
    in
    List.find_map items ~f:(loop ~path:[])
  ;;

  let find_submenu (items : ('effect, 'item) t list) (target_path : string list)
    : ('effect, 'item) t list
    =
    match target_path with
    | [] -> items
    | _ :: _ ->
      find_map items ~f:(fun ~path item ->
        match item with
        | Submenu { key; items; _ }
          when is_selectable item && List.equal String.equal (path @ [ key ]) target_path
          -> Some items
        | Single _ | Section _ | Inert _ | Submenu _ -> None)
      |> Option.value ~default:[]
  ;;

  let first (items : ('effect, 'item) t list) : ('effect, 'item) t option =
    find_map items ~f:(fun ~path:_ item -> selectable_item item)
  ;;

  let rec last (items : ('effect, 'item) t list) : ('effect, 'item) t option =
    List.find_map (List.rev items) ~f:(function
      | Section { items; _ } -> last items
      | item -> selectable_item item)
  ;;

  let next items ~after =
    let seen_after = ref false in
    find_map items ~f:(fun ~path item ->
      match List.is_empty path, !seen_after with
      | false, _ -> None
      | true, true -> selectable_item item
      | true, false ->
        (match key item with
         | Some key when String.equal key after -> seen_after := true
         | Some _ | None -> ());
        None)
  ;;

  let prev items ~before =
    let prev_selectable = ref None in
    find_map items ~f:(fun ~path item ->
      match List.is_empty path with
      | false -> None
      | true ->
        (match key item with
         | Some key when String.equal key before -> !prev_selectable
         | Some _ | None ->
           (match selectable_item item with
            | Some item -> prev_selectable := Some item
            | None -> ());
           None))
  ;;
end

let current_and_path active =
  match List.rev active with
  | [] -> None
  | current :: path -> Some (current, List.rev path)
;;

let find_active_item (menu : _ Item.t list) active =
  let find menu key' =
    Item.find_map menu ~f:(fun ~path item ->
      match List.is_empty path with
      | false -> None
      | true ->
        let%bind.Option selectable_key = Item.selectable_key item in
        Option.some_if (String.equal selectable_key key') item)
  in
  match current_and_path active with
  | None -> None
  | Some (active, []) -> find menu active
  | Some (active, path) ->
    let submenu = Item.find_submenu menu path in
    find submenu active
;;

let find_first_item_key_in_menu menu =
  let%bind.Option item = Item.first menu in
  Item.key item
;;

let find_last_item_key_in_menu menu =
  let%bind.Option item = Item.last menu in
  Item.key item
;;

let find_next_item_key_in_menu menu current =
  let%bind.Option item = Item.next menu ~after:current in
  Item.key item
;;

let find_prev_item_key_in_menu menu current =
  let%bind.Option item = Item.prev menu ~before:current in
  Item.key item
;;

let apply_action' ctx menu active action =
  match action with
  | `Set path -> path
  | `Enter ->
    (match find_active_item menu active with
     | Some (Single { on_click; _ }) ->
       Bonsai.Apply_action_context.schedule_event ctx on_click;
       active
     | Some (Submenu { items; _ }) ->
       (match find_first_item_key_in_menu items with
        | Some key -> List.append active [ key ]
        | None -> active)
     | _ -> active)
  | `Up ->
    (match current_and_path active with
     | None -> Option.to_list (find_last_item_key_in_menu menu)
     | Some (current, path) ->
       let submenu = Item.find_submenu menu path in
       let next =
         match find_prev_item_key_in_menu submenu current with
         | Some next -> next
         | None -> Option.value (find_last_item_key_in_menu submenu) ~default:current
       in
       List.append path [ next ])
  | `Down ->
    (match current_and_path active with
     | None -> Option.to_list (find_first_item_key_in_menu menu)
     | Some (current, path) ->
       let submenu = Item.find_submenu menu path in
       let next =
         match find_next_item_key_in_menu submenu current with
         | Some next -> next
         | None -> Option.value (find_first_item_key_in_menu submenu) ~default:current
       in
       List.append path [ next ])
  | `Left ->
    (match active with
     | [] | [ _ ] -> active
     | _ :: _ :: _ -> List.drop_last_exn active)
  | `Right ->
    (match find_active_item menu active with
     | Some (Submenu { items; _ }) ->
       (match find_first_item_key_in_menu items with
        | Some key -> List.append active [ key ]
        | None -> active)
     | _ -> active)
;;

let apply_action ctx menu active action =
  match (menu : _ Bonsai.Computation_status.t) with
  | Active menu -> apply_action' ctx menu active action
  | Inactive -> active
;;

type key =
  [ `Enter
  | `Up
  | `Down
  | `Left
  | `Right
  ]

type action =
  [ `Set of string list
  | key
  ]

type 'item t =
  { menu : (unit, 'item) Item.t list
  ; active : string list
  ; inject : action -> unit Effect.t
  }

let component menu graph =
  let active, inject =
    Bonsai.state_machine_with_input ~default_model:[] ~apply_action menu graph
  in
  let%arr menu and active and inject in
  { menu; active; inject }
;;

let active_path { active; _ } = active
let active_item { menu; active; _ } = Effect.of_sync_fun (find_active_item menu) active
let set_active_path { inject; _ } path = inject (`Set path)
let key_down { inject; _ } (key : key) = inject (key :> action)

module For_testing = struct
  let next = Item.next
  let prev = Item.prev
end
