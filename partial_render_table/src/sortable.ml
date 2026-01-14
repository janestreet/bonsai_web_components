open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_protocol

type 'col_id t =
  { order : 'col_id Order.t
  ; inject : 'col_id Order.Action.t -> unit Effect.t
  ; col_id_equal : 'col_id -> 'col_id -> bool
  }

let order t = t.order
let inject t = t.inject

let state
  ?(initial_order = Bonsai.return Order.default)
  ~equal:col_id_equal
  ()
  (local_ graph)
  =
  let equal = Order.equal col_id_equal in
  let order, inject =
    Bonsai_extra.State_machine.state_machine0_dynamic_model
      ~equal
      ~model:(`Given initial_order)
      ~apply_action:(fun _ -> Order.apply_action ~equal:col_id_equal)
      graph
  in
  let order = Bonsai.cutoff ~equal order in
  let%arr order and inject in
  { order; inject; col_id_equal }
;;

module Header = struct
  let assoc_findi ~f list =
    match List.findi list ~f:(fun _i (key, _) -> f key) with
    | None -> None
    | Some (index, (_k, v)) -> Some (index, v)
  ;;

  module Expert = struct
    let default_click_handler
      ?(multisort_columns_when = `Shift_click)
      ?test_selector
      { order; inject; col_id_equal }
      ~column_id
      ~sortable
      f
      =
      let handle_click =
        Vdom.Attr.on_click (fun mouse_event ->
          let shift = Js_of_ocaml.Js.to_bool mouse_event##.shiftKey in
          let ctrl = Js_of_ocaml.Js.to_bool mouse_event##.ctrlKey in
          let should_multisort =
            match multisort_columns_when with
            | `Shift_click -> shift
            | `Ctrl_click -> ctrl
            | `Shift_or_ctrl_click -> shift || ctrl
            | `Disabled -> false
          in
          if should_multisort
          then inject (Add_sort (column_id, Asc_to_desc_to_none))
          else inject (Set_sort (column_id, Asc_to_desc_to_none)))
      in
      let (sort_state : Sort_state.t) =
        if not sortable
        then Not_sortable
        else (
          let col_state = assoc_findi ~f:(col_id_equal column_id) order in
          match col_state with
          | None -> Not_sorted
          | Some (index, dir) ->
            if List.length order = 1
            then Single_sort dir
            else Multi_sort { dir; index = index + 1 })
      in
      let header_node = f sort_state in
      Table_view.Header_label.wrap_clickable
        ?test_selector
        ~sortable
        ~handle_click
        header_node
    ;;
  end

  let with_icon = Table_view.Header_label.wrap_with_icon

  (* Not in `Table_view.ml`, because we don't add theming to legacy implementations. *)
  module Legacy = struct
    module Icons = struct
      (** White Diamond symbol (U+25C7) *)
      let neutral = "◇ "

      (** Diamond with Top Black Half symbol (U+2B18) *)
      let ascending = "⬘ "

      (** Diamond with Bottom Black Half symbol (U+2B19) *)
      let descending = "⬙ "
    end

    let wrap_with_icon (label : Vdom.Node.t) (sort_spec : Sort_state.t) =
      let get_icon = function
        | `None -> Icons.neutral
        | `Asc -> Icons.ascending
        | `Desc -> Icons.descending
      in
      let render ~dir = Vdom.Node.span [ Vdom.Node.text (get_icon dir); label ] in
      match sort_spec with
      | Not_sortable -> label
      | Not_sorted -> render ~dir:`None
      | Single_sort dir | Multi_sort { dir; _ } -> render ~dir
    ;;
  end
end

module Wrap_header = struct
  type 'column_id sortable = 'column_id t

  type 'column_id basic =
    'column_id sortable
    -> is_sortable:('column_id -> bool)
    -> column_id:'column_id
    -> Vdom.Node.t
    -> Vdom.Node.t

  let clickable_with_icon ?extra_attrs ?sort_indicator_attrs ?multisort_columns_when () =
    let basic_wrap_header_function sortable ~is_sortable ~column_id content =
      let is_sortable = is_sortable column_id in
      Header.Expert.default_click_handler
        ?multisort_columns_when
        sortable
        ~column_id
        ~sortable:is_sortable
        (Header.with_icon ?extra_attrs ?sort_indicator_attrs content)
    in
    basic_wrap_header_function
  ;;

  let clickable_with_icon_deprecated ?multisort_columns_when () =
    let wrap_header sortable ~is_sortable ~column_id content =
      let is_sortable = is_sortable column_id in
      Header.Expert.default_click_handler
        ?multisort_columns_when
        sortable
        ~column_id
        ~sortable:is_sortable
        (Header.Legacy.wrap_with_icon content)
    in
    wrap_header
  ;;

  let clickable_no_icon ?multisort_columns_when () =
    let wrap_header sortable ~is_sortable ~column_id node =
      let is_sortable = is_sortable column_id in
      Header.Expert.default_click_handler
        ?multisort_columns_when
        sortable
        ~column_id
        ~sortable:is_sortable
        (fun _ -> node)
    in
    wrap_header
  ;;

  let none _sortable ~is_sortable:_ ~column_id:_ node = node
end
