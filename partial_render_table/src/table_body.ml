open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

module For_testing = struct
  type cell =
    { cell_focused : bool
    ; view : Vdom.Node.t
    }

  type row =
    { id : Opaque_map.Key.t
    ; row_focused : bool
    ; cells : cell list
    }

  type t =
    { column_names : Vdom.Node.t list list
    ; rows : row list
    ; rows_before : int
    ; rows_after : int
    ; num_filtered : int
    ; num_unfiltered : int
    }
end

(* Defining the function out here so that focused_col_in_row doesn't trigger recomputes
   whenever Cell_focused changes and we are not in cell focused mode
*)
let return_none_1 _ = None

(* Defining the function out here so that is_row_focused doesn't trigger recomputes
   whenever Row_focused changes and we are not in cell focused mode
*)
let return_false_1 _ = false

module Inner_focus_state = struct
  type 'column_id t =
    | Nothing_focused
    | Row_is_focused
    | Cell_in_row_is_focused of 'column_id
end

let rows
  (type key cmp column_id column_id_cmp kind)
  ~themed_attrs
  ~resize_column_widths_to_fit
  ~(key_comparator : (key, cmp) Comparator.Module.t)
  ~(column_id_comparator : (column_id, column_id_cmp) Comparator.Module.t)
  ~row_height
  ~(leaves : column_id Header_tree.leaf list Bonsai.t)
  ~(col_widths : (column_id, Column_size.t, column_id_cmp) Map.t Bonsai.t)
  ~(visually_focused : (key, column_id, kind) Focus.focused Bonsai.t)
  ~(on_cell_click : (key -> column_id -> unit Effect.t) Bonsai.t)
  ~extra_row_attrs
  (cells : (key * (column_id * Vdom.Node.t) list) Opaque_map.t Bonsai.t)
  (local_ graph)
  =
  let module Key_cmp = (val key_comparator) in
  let module Col_cmp = (val column_id_comparator) in
  (* It's tempting to use [Bonsai.Map.unordered_fold] over the [col_widths], but some
     columns in [col_widths] may not actually be present in the table. *)
  let row_width =
    let%arr col_widths and leaves in
    List.sum (module Float) leaves ~f:(fun leaf ->
      match Map.find col_widths leaf.column_id with
      | Some (Visible { width_px }) -> width_px
      | None | Some (Hidden _) -> 0.0)
  in
  let col_styles =
    let%arr themed_attrs
    and (`Px row_height) = row_height
    and resize_column_widths_to_fit
    and col_widths
    and leaves in
    Table_view.Cell.Col_styles.create
      column_id_comparator
      ~themed_attrs
      ~resize_column_widths_to_fit
      ~row_height
      ~col_widths
      ~leaves
  in
  let row_styles =
    let%arr (`Px row_height) = row_height
    and resize_column_widths_to_fit
    and row_width in
    Table_view.Row.Styles.create ~row_height ~row_width ~resize_column_widths_to_fit
  in
  let is_row_focused =
    let%arr visually_focused in
    match visually_focused with
    | Nothing_focused | Cell_focused _ -> return_false_1
    | Row_focused k ->
      fun key -> Comparable.equal (Comparator.compare Key_cmp.comparator) k key
  in
  let focused_column_in_row =
    let%arr visually_focused in
    match visually_focused with
    | Nothing_focused | Row_focused _ -> return_none_1
    | Cell_focused (k, c) ->
      fun key ->
        Option.some_if (Comparable.equal (Comparator.compare Key_cmp.comparator) k key) c
  in
  Bonsai.assoc
    (module Opaque_map.Key)
    cells
    ~f:(fun _ key_and_cells (local_ _graph) ->
      let focus =
        let%arr is_row_focused
        and focused_column_in_row
        and key, _ = key_and_cells in
        match focused_column_in_row key with
        | None when is_row_focused key -> Inner_focus_state.Row_is_focused
        | None -> Nothing_focused
        | Some focused_column_in_row -> Cell_in_row_is_focused focused_column_in_row
      in
      let cells =
        let%arr on_cell_click
        and focus
        and col_styles
        and key, cells = key_and_cells
        and themed_attrs
        and resize_column_widths_to_fit in
        let col_styles column_id = (Staged.unstage col_styles) column_id in
        List.map cells ~f:(fun (column_id, cell) ->
          let cell_is_focused =
            match focus with
            | Cell_in_row_is_focused focused_column_id ->
              Comparable.equal
                (Comparator.compare Col_cmp.comparator)
                column_id
                focused_column_id
            | _ -> false
          in
          Table_view.Cell.view
            themed_attrs
            ~is_focused:cell_is_focused
            ~col_styles:(col_styles column_id)
            ~on_cell_click:(Effect.lazy_ (lazy (on_cell_click key column_id)))
            ~resize_column_widths_to_fit
            cell)
      in
      let%arr themed_attrs
      and key_and_cells
      and cells
      and resize_column_widths_to_fit
      and focus
      and row_styles
      and extra_row_attrs in
      (* Any change to [cells] will always cause a recomputation. If we don't ignore it,
         we save a cutoff node per row. *)
      let key, _ = key_and_cells in
      let extra_attrs = extra_row_attrs key in
      let is_focused =
        match focus with
        | Row_is_focused -> true
        | _ -> false
      in
      let has_focused_cell =
        match focus with
        | Cell_in_row_is_focused _ -> true
        | _ -> false
      in
      Table_view.Row.view
        themed_attrs
        ~extra_attrs
        ~styles:row_styles
        ~is_focused
        ~has_focused_cell
        ~resize_column_widths_to_fit
        cells)
    graph
;;

let component
  (type key data cmp col col_cmp kind)
  ~themed_attrs
  ~resize_column_widths_to_fit
  ~(key_comparator : (key, cmp) Comparator.Module.t)
  ~(column_id_comparator : (col, col_cmp) Comparator.Module.t)
  ~row_height
  ~(headers : col Header_tree.t Bonsai.t)
  ~(leaves : col Header_tree.leaf list Bonsai.t)
  ~(assoc :
      (key * data) Opaque_map.t Bonsai.t
      -> local_ Bonsai.graph
      -> (key * (col * Vdom.Node.t) list) Opaque_map.t Bonsai.t)
  ~column_widths
  ~(visually_focused : (key, col, kind) Focus.focused Bonsai.t)
  ~on_cell_click
  ~extra_row_attrs
  (collated : (key, data) Collated.t Bonsai.t)
  (input : (key * data) Opaque_map.t Bonsai.t)
  : local_ Bonsai.graph -> (Table_view.Body.t * For_testing.t Lazy.t) Bonsai.t
  =
  fun (local_ graph) ->
  let padding_top_and_bottom =
    let%arr collated
    and (`Px row_height) = row_height in
    let padding_top = Collated.num_before_range collated * row_height in
    let padding_bottom = Collated.num_after_range collated * row_height in
    padding_top, padding_bottom
  in
  let cells = assoc input graph in
  let rows =
    rows
      ~themed_attrs
      ~resize_column_widths_to_fit
      ~key_comparator
      ~column_id_comparator
      ~row_height
      ~leaves
      ~col_widths:column_widths
      ~visually_focused
      ~on_cell_click
      ~extra_row_attrs
      cells
      graph
  in
  let view =
    let rows =
      Bonsai.Map.rekey
        ~comparator:(module Table_view.Body.Body_row_key)
        ~f:(fun ~key ~data:_ -> Row key)
        rows
        graph
    in
    let%arr padding_top, padding_bottom = padding_top_and_bottom
    and themed_attrs
    and resize_column_widths_to_fit
    and rows in
    Table_view.Body.view
      themed_attrs
      ~padding_top
      ~padding_bottom
      ~rows
      ~resize_column_widths_to_fit
  in
  let for_testing =
    let%arr cells and collated and visually_focused and headers in
    lazy
      (let column_names = Header_tree.column_names headers in
       { For_testing.column_names
       ; rows =
           List.map (Map.to_alist cells) ~f:(fun (id, (key, view)) ->
             let module Key_cmp = (val key_comparator) in
             let module Col_cmp = (val column_id_comparator) in
             let row_focused =
               match visually_focused with
               | Nothing_focused | Cell_focused _ -> false
               | Row_focused k ->
                 Comparable.equal (Comparator.compare Key_cmp.comparator) k key
             in
             let cells =
               List.map view ~f:(fun (column, view) ->
                 let cell_focused =
                   match visually_focused with
                   | Nothing_focused | Row_focused _ -> false
                   | Cell_focused (k, c) ->
                     Comparable.equal (Comparator.compare Key_cmp.comparator) key k
                     && Comparable.equal (Comparator.compare Col_cmp.comparator) c column
                 in
                 { For_testing.view; cell_focused })
             in
             { For_testing.id; row_focused; cells })
       ; rows_before = Collated.num_before_range collated
       ; rows_after = Collated.num_after_range collated
       ; num_filtered = Collated.num_filtered_rows collated
       ; num_unfiltered = Collated.num_unfiltered_rows collated
       })
  in
  let%arr view and for_testing in
  view, for_testing
;;
