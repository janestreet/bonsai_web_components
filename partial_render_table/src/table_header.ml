open! Core
open! Bonsai_web
open! Js_of_ocaml
open! Incr_map_collate
open! Bonsai.Let_syntax

module Acc = struct
  type t = { level_map : Table_view.Header.Header_cell.t list Int.Map.t }

  let empty = { level_map = Int.Map.empty }

  let visit_leaf { level_map } ~level ~node =
    let level_map = Map.add_multi level_map ~key:level ~data:node in
    { level_map }
  ;;

  let visit_non_leaf { level_map } ~level ~node =
    let level_map = Map.add_multi level_map ~key:level ~data:node in
    { level_map }
  ;;

  let finalize ~themed_attrs { level_map } =
    level_map
    |> Map.data
    |> List.map ~f:(fun seq ->
      Table_view.Header.Header_row.view themed_attrs (List.rev seq))
  ;;
end

let rec render_header
  ~column_id_equal
  ~focused_column
  ~themed_attrs
  ~level
  ~column_widths
  ~set_column_width
  ~set_column_width_for_reporting
  ~resize_column_widths_to_fit
  ~acc
  header
  =
  let recurse =
    render_header
      ~column_id_equal
      ~focused_column
      ~themed_attrs
      ~level:(level + 1)
      ~column_widths
      ~set_column_width
      ~set_column_width_for_reporting
      ~resize_column_widths_to_fit
  in
  let recurse_no_level_change =
    render_header
      ~column_id_equal
      ~focused_column
      ~themed_attrs
      ~level
      ~column_widths
      ~set_column_width
      ~set_column_width_for_reporting
      ~resize_column_widths_to_fit
  in
  match header with
  | Header_tree.Leaf { visible; leaf_header; resizable; initial_width; column_id } ->
    let node =
      let column_width =
        match Map.find column_widths column_id with
        | Some (Column_size.Visible { width_px = width })
        | Some (Hidden { prev_width_px = Some width }) -> `Px_float width
        | None | Some (Hidden { prev_width_px = None }) -> initial_width
      in
      let focused =
        Option.value_map focused_column ~default:false ~f:(column_id_equal column_id)
      in
      Table_view.Header.Header_cell.leaf_view
        themed_attrs
        ~column_width
        ~resize_column_widths_to_fit
        ~set_column_width:(set_column_width ~column_id)
        ~set_column_width_for_reporting:(set_column_width_for_reporting ~column_id)
        ~focused
        ~visible
        ~resizable
        ~label:leaf_header
        ()
    in
    Acc.visit_leaf acc ~level ~node
  | Spacer inside ->
    let node =
      Table_view.Header.Header_cell.spacer_view
        themed_attrs
        ~colspan:(Header_tree.colspan header)
        ~resize_column_widths_to_fit
        ()
    in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    recurse ~acc inside
  | Group { children; group_header } ->
    let node =
      Table_view.Header.Header_cell.group_view
        themed_attrs
        ~colspan:(Header_tree.colspan header)
        ~resize_column_widths_to_fit
        ~label:group_header
        ()
    in
    let acc = Acc.visit_non_leaf acc ~level ~node in
    List.fold children ~init:acc ~f:(fun acc -> recurse ~acc)
  | Organizational_group children ->
    List.fold children ~init:acc ~f:(fun acc -> recurse_no_level_change ~acc)
;;

let render_header
  ~column_id_equal
  ~focused_column
  ~themed_attrs
  headers
  ~column_widths
  ~set_column_width
  ~set_column_width_for_reporting
  ~resize_column_widths_to_fit
  =
  headers
  |> render_header
       ~column_id_equal
       ~focused_column
       ~themed_attrs
       ~level:0
       ~column_widths
       ~set_column_width
       ~set_column_width_for_reporting
       ~resize_column_widths_to_fit
       ~acc:Acc.empty
  |> Acc.finalize ~themed_attrs
;;

let component
  (type column_id column_id_cmp)
  ~column_id_equal
  ~themed_attrs
  ~resize_column_widths_to_fit
  ~focused_column
  (headers : column_id Header_tree.t Bonsai.t)
  ~(column_widths : (column_id, Column_size.t, column_id_cmp) Map.t Bonsai.t)
  ~(set_column_width :
      (column_id:column_id -> [< `Px_float of float ] -> unit Effect.t) Bonsai.t)
  ~(set_column_width_for_reporting :
      (column_id:column_id -> [< `Px_float of float ] -> unit Effect.t) Bonsai.t)
  ~set_header_client_rect
  (local_ _graph)
  =
  let%arr set_column_width
  and set_column_width_for_reporting
  and focused_column
  and set_header_client_rect
  and headers
  and column_widths
  and resize_column_widths_to_fit
  and themed_attrs in
  let header_rows =
    render_header
      headers
      ~column_id_equal
      ~focused_column
      ~themed_attrs
      ~set_column_width
      ~set_column_width_for_reporting
      ~column_widths
      ~resize_column_widths_to_fit
  in
  Table_view.Header.view
    themed_attrs
    ~set_header_client_rect
    ~resize_column_widths_to_fit
    header_rows
;;
