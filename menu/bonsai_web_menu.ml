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
        ; item : 'item
        ; items : ('effect, 'item) t list
        }

  let rec sexp_of_t sexp_of_a sexp_of_item : _ t -> Sexp.t = function
    | Single { key; _ } -> Atom key
    | Section { items; _ } -> List (List.map items ~f:(sexp_of_t sexp_of_a sexp_of_item))
    | Inert item -> sexp_of_item item
    | Submenu { key; _ } -> Atom (key ^ "...")
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

  let find_map
    (items : ('effect, 'item) t list)
    ~(f : ('effect, 'item) t option -> ('effect, 'item) t -> 'b option)
    : 'b option
    =
    let rec loop items prev =
      match items with
      | (Single { disabled = true; _ } | Submenu { items = []; _ } | Inert _) :: rest ->
        (* Skip disabled and inert items. *)
        loop rest prev
      | (Single { disabled = false; _ } as current) :: rest ->
        (* Walk items in order *)
        (match f prev current with
         | Some value -> `Some value
         | None -> loop rest (Some current))
      | Section { items; _ } :: rest ->
        (* Iterate any items inside a section first, then following items *)
        (match loop items prev with
         | `Some _ as some -> some
         | `Last prev -> loop rest (Some prev)
         | `Empty -> loop rest prev)
      | (Submenu { items = _ :: _; _ } as current) :: rest ->
        (* First visit the submenu item itself, then the remainder. *)
        (match f prev current with
         | Some value -> `Some value
         | None -> loop rest (Some current))
      | [] ->
        (match prev with
         | Some prev -> `Last prev
         | None -> `Empty)
    in
    match loop items None with
    | `Some value -> Some value
    | `Empty | `Last _ -> None
  ;;

  let rec find_submenu (items : ('effect, 'item) t list) (path : string list)
    : ('effect, 'item) t list
    =
    match path with
    | [] -> items
    | key' :: path ->
      let submenu_at_key =
        find_map items ~f:(fun _ item ->
          match item with
          | Submenu { key; items; _ } when String.equal key key' -> Some items
          | _ -> None)
      in
      (match submenu_at_key with
       | Some menu -> find_submenu menu path
       | None -> [])
  ;;

  let first (items : ('effect, 'item) t list) : ('effect, 'item) t option =
    find_map items ~f:(fun _ item ->
      match item with
      | Single _ | Submenu _ -> Some item
      | Section _ | Inert _ -> None)
  ;;

  let rec last (items : ('effect, 'item) t list) : ('effect, 'item) t option =
    match items with
    | [] | [ (Single { disabled = true; _ } | Submenu { items = []; _ } | Inert _) ] ->
      None
    | [ (Single { disabled = false; _ } as item) ] -> Some item
    | [ (Submenu { items = _ :: _; _ } as item) ] -> Some item
    | [ Section { items; _ } ] -> last items
    | first :: rest ->
      (match last rest with
       | None -> last [ first ]
       | Some _ as item -> item)
  ;;
end

let current_and_path active =
  match List.rev active with
  | [] -> None
  | current :: path -> Some (current, List.rev path)
;;

let find_active_item (menu : _ Item.t list) active =
  let find menu key' =
    Item.find_map menu ~f:(fun _ item ->
      match item with
      | Single { key; _ } | Submenu { key; _ } ->
        if String.equal key key' then Some item else None
      | _ -> None)
  in
  match current_and_path active with
  | None -> None
  | Some (active, []) -> find menu active
  | Some (active, path) ->
    let submenu = Item.find_submenu menu path in
    find submenu active
;;

let find_first_item_key_in_menu menu =
  match%bind.Option Item.first menu with
  | Single { key; _ } | Submenu { key; _ } -> Some key
  | Section _ | Inert _ -> None
;;

let find_last_item_key_in_menu menu =
  match%bind.Option Item.last menu with
  | Single { key; _ } | Submenu { key; _ } -> Some key
  | Section _ | Inert _ -> None
;;

let find_next_item_key_in_menu menu current =
  Item.find_map menu ~f:(fun prev item ->
    let prev_is_current =
      match prev with
      | None | Some (Section _) | Some (Inert _) -> false
      | Some (Single { key; _ } | Submenu { key; _ }) -> String.equal key current
    in
    match item with
    | (Single { key; _ } | Submenu { key; _ }) when prev_is_current -> Some key
    | _ -> None)
;;

let find_prev_item_key_in_menu menu current =
  Item.find_map menu ~f:(fun prev item ->
    let item_is_current =
      match item with
      | Section _ -> false
      | Inert _ -> false
      | Single { key; _ } | Submenu { key; _ } -> String.equal key current
    in
    match prev with
    | Some (Single { key; _ } | Submenu { key; _ }) when item_is_current -> Some key
    | _ -> None)
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
