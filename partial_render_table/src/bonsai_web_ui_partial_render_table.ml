open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Incr_map_collate
open Bonsai_web_ui_partial_render_table_protocol
module Bbox = Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox
module Order = Order
module Sort_state = Sort_state
module Sort_kind = Sort_kind
module Sortable = Sortable
module Focus_by_row = Focus.By_row
module Focus_by_cell = Focus.By_cell
module Scroll = Bonsai_web_ui_scroll_utilities
module Indexed_column_id = Old_columns.Indexed_column_id
module Which_styling = Table_view.Which_styling

module For_testing = struct
  module Table_body = Table_body.For_testing

  type t = { body : Table_body.t }
end

let default_preload = 70

module Column_structure = Columns.Column_structure
module Render_cell = Columns.Render_cell

module Expert = struct
  module Focus = struct
    include Focus
    include Kind
  end

  module Result = struct
    type ('focus, 'column_id) t =
      { view : Vdom.Node.t
      ; range : int * int
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      ; column_widths : ('column_id * [ `Px_float of float ]) list Lazy.t
      }
    [@@deriving fields ~getters]
  end

  module Columns = struct
    type ('key, 'data, 'column_id) t = ('key, 'data, 'column_id) Column_intf.t

    include Columns.Expert
    module Indexed_column_id = Indexed_column_id
    module Dynamic_cells = Old_columns.Dynamic_cells
    module Dynamic_columns = Old_columns.Dynamic_columns
    module Dynamic_experimental = Old_columns.Dynamic_experimental
  end

  module Row_height_model = struct
    type t = [ `Px of int ] [@@deriving sexp, equal]
  end

  let print_in_tests f =
    match Bonsai_web.am_running_how with
    | `Browser | `Browser_test | `Browser_benchmark | `Node | `Node_benchmark ->
      Effect.Ignore
    | `Node_test | `Node_jsdom_test ->
      Effect.of_sync_fun (fun () -> print_endline (f ())) ()
  ;;

  let column_width_tracker
    ?(round_column_width = Float.round_decimal ~decimal_digits:2)
    ~sexp_of_model
    ~equal
    ~default_model
    graph
    =
    let column_widths, set_column_width =
      Bonsai.state_machine
        graph
        ~sexp_of_model
        ~equal
        ~default_model
        ~apply_action:
          (fun
            (_ : _ Bonsai.Apply_action_context.t) model (column_id, `Px_float width) ->
          (* While checking for float equality is usually not a good idea, this is meant
             to handle the specific case when a column has "display:none", in which case
             the width will be exactly 0.0, so there is no concern about float rounding
             errors. *)
          Map.update model column_id ~f:(fun prev ->
            if Float.equal width 0.0
            then (
              match prev with
              | None -> Hidden { prev_width_px = None }
              | Some (Column_size.Visible { width_px }) ->
                Hidden { prev_width_px = Some width_px }
              | Some (Hidden _ as prev) -> prev)
            else (
              let rounded = round_column_width width in
              Visible { width_px = rounded })))
    in
    let column_widths = Bonsai.cutoff ~equal column_widths in
    let set_column_width =
      let%arr set_column_width in
      fun ~column_id width -> set_column_width (column_id, width)
    in
    column_widths, set_column_width
  ;;

  let implementation
    (type column_id key presence data cmp)
    ?extra_row_attrs
    ~styling
    ~resize_column_widths_to_fit
    ?round_column_width
    ~preload_rows
    ~(wrap_header : (column_id:column_id -> Vdom.Node.t -> Vdom.Node.t) Bonsai.t)
    (key_comparator : (key, cmp) Comparator.Module.t)
    ~(focus : (_, presence, key, column_id) Focus.Kind.t)
    ~row_height
    ~(columns : (key, data, column_id) Column_intf.t)
    (collated : (key, data) Collated.t Bonsai.t)
    (local_ graph)
    =
    let (T { value; vtable; column_id }) = columns in
    let module T = (val vtable) in
    let headers = T.headers ~wrap_header value graph in
    let assoc cells = T.instantiate_cells value key_comparator cells in
    let extra_row_attrs =
      match extra_row_attrs with
      | None -> Bonsai.return (fun _ -> [])
      | Some extra_row_attrs -> extra_row_attrs
    in
    let themed_attrs =
      Table_view.Themed.resolve ~resize_column_widths_to_fit styling graph
    in
    let row_height =
      let%arr (`Px row_height) = row_height in
      `Px (Int.max 1 row_height)
    in
    let focus_kind = focus in
    let input_map = Bonsai.map ~f:Collated.to_opaque_map collated in
    let private_body_classname =
      let path = Bonsai.path_id graph in
      let%arr path in
      "partial-render-table-body-" ^ path
    in
    let table_body_selector =
      let%arr private_body_classname in
      "." ^ private_body_classname
    in
    let header_client_rect, set_header_client_rect = Bonsai.state_opt graph in
    let header_client_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] header_client_rect
    in
    let set_header_client_rect =
      let%arr set_header_client_rect in
      fun b -> set_header_client_rect (Some b)
    in
    let table_body_visible_rect, set_table_body_visible_rect =
      Bonsai.state_opt ~equal:[%equal: Bbox.t] graph
    in
    let table_body_visible_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] table_body_visible_rect
    in
    let table_body_client_rect, set_table_body_client_rect =
      Bonsai.state_opt graph ~equal:[%equal: Bbox.t]
    in
    let table_body_client_rect =
      Bonsai.cutoff ~equal:[%equal: Bbox.t option] table_body_client_rect
    in
    let module Column_cmp = struct
      include (val column_id)

      let sexp_of_t = Comparator.sexp_of_t comparator
    end
    in
    let module Column_widths_model = struct
      type t = Column_size.t Map.M(Column_cmp).t [@@deriving sexp_of, equal]
    end
    in
    let column_widths, set_column_width =
      column_width_tracker
        ?round_column_width
        graph
        ~sexp_of_model:[%sexp_of: Column_widths_model.t]
        ~equal:[%equal: Column_widths_model.t]
        ~default_model:(Map.empty (module Column_cmp))
    in
    let column_widths_for_reporting, set_column_width_for_reporting =
      (* This "for_reporting" map is kept separately so that the
         [resize_column_widths_to_fit] can keep the primary [column_widths] value for
         explicitly set column widths, while this tracker maintains all the
         currently-known sizes. *)
      column_width_tracker
        ?round_column_width
        graph
        ~sexp_of_model:[%sexp_of: Column_widths_model.t]
        ~equal:[%equal: Column_widths_model.t]
        ~default_model:(Map.empty (module Column_cmp))
    in
    let set_column_width =
      let%arr set_column_width and set_column_width_for_reporting in
      fun ~column_id size ->
        (* This has to contain both of these otherwise the width tracker will not work
           properly *)
        Effect.Many
          [ set_column_width ~column_id size
          ; set_column_width_for_reporting ~column_id size
          ]
    in
    let row_count = collated >>| Collated.num_filtered_rows in
    let header_height_px =
      match%arr header_client_rect with
      | None -> 0.0
      | Some header_rect -> Bbox.height header_rect
    in
    let range_without_preload =
      (* The goal of this value is to track the index range of the rows that would be
         visible in the table if the table were full. Usually this would be as easy as
         looking at the table_body_visible_rect, but we also account for the header
         occluding some of the first rows of the table. For that, we need to look at the
         client-rects for the body and the header and subtract their overlap. *)
      let%arr table_body_visible_rect
      and table_body_client_rect
      and header_client_rect
        (* We need to know about resize_column_widths_to_fit because the sticky header
           takes up 0 height in the table container, so it doesn't technically occlude
           anything initially. *)
      and resize_column_widths_to_fit
      and header_height_px in
      fun (`Px row_height_px) ->
        let row_height_px = Float.of_int row_height_px in
        match table_body_visible_rect, table_body_client_rect, header_client_rect with
        | Some { min_y = body_min_y; max_y = body_max_y; _ }, _, None ->
          (* if we don't have the header-height yet, just assume that there's no overlap. *)
          let low = body_min_y /. row_height_px in
          let high = (body_max_y -. row_height_px +. 2.) /. row_height_px in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | ( Some { min_y = table_visible_top; max_y = table_visible_bottom; _ }
          , Some { min_y = table_absolute_top; _ }
          , Some { max_y = header_visible_bottom; _ } ) ->
          let low_offset, high_offset =
            let header_offset =
              Float.min header_height_px (header_visible_bottom -. table_absolute_top)
            in
            match resize_column_widths_to_fit with
            (* resize_column_widths_to_fit:false shifts the top of the header down in
               position due to the attr that calculates visible client rect being on an
               element that only contains the header *)
            | false -> header_offset, 0.
            (* When resize_column_widths_to_fit:true, the header is in the same container
               as the body. That container is where the visible client rect attr is
               attached, so the client rect also considers the header as part of what is
               visible. Due to this, we have to subtract the header height from the bottom
               of the rect, as we're reducing the visible body height and not its
               position.
            *)
            | true -> 0., header_offset *. -1.
          in
          let low = (table_visible_top +. low_offset) /. row_height_px in
          let high =
            (table_visible_bottom +. high_offset -. row_height_px +. 2.) /. row_height_px
          in
          Some (Float.(to_int (round_nearest low)), Float.(to_int (round_nearest high)))
        | _ -> None
    in
    let midpoint_of_container =
      let%arr table_body_visible_rect in
      match table_body_visible_rect with
      | None -> 0.0, 0.0
      | Some rect -> (rect.max_x +. rect.min_x) /. 2.0, (rect.max_y -. rect.min_y) /. 2.0
    in
    let get_y_px_of_row_index =
      let%arr header_height_px
      and range_without_preload
      and resize_column_widths_to_fit
      and (`Px row_height_px) = row_height in
      fun index ->
        let range_start, range_end =
          (* [range_without_preload] can be [None] if the number of rows in the table
             shrinks such that the table becomes invisible. By providing a small index, we
             ensure that the padding around the table does not force the page to remain
             large enough for the existing scroll position. With the padding removed, the
             browser should automatically reduce the range of the scrollbar, and possibly
             bringing the table back in view, which would quickly correct this value to
             something more useful. *)
          range_without_preload (`Px row_height_px) |> Option.value ~default:(0, 1)
        in
        let row_height_px = Float.of_int row_height_px in
        let to_top =
          let header_offset =
            match resize_column_widths_to_fit with
            (* scrolling this row to the top of the display involves scrolling to a pixel
               that is actually [header_height] _above_ the target row. *)
            (* resize_column_widths_to_fit:false shifts the top of the header down in
               position due to the attr that calculates visible client rect being on an
               element that only contains the header *)
            | false -> header_height_px
            (* When resize_column_widths_to_fit:true, the header is in the same container
               as the body. That container is where the visible client rect attr is
               attached, so the client rect also considers the header as part of what is
               visible. Due to this, we have to subtract the header height from the bottom
               of the rect, as we're reducing the visible body height and not its
               position.
            *)
            | true -> 0.
          in
          (row_height_px *. Float.of_int index) -. header_offset
        in
        let to_bottom =
          let header_offset =
            match resize_column_widths_to_fit with
            (* In resize_column_widths_to_fit:true, the header is in the same container as
               the body, so we need to account for the headers height in row offset
               calculations *)
            | true -> header_height_px
            | false -> 0.
          in
          (* scroll to the bottom of this row means scrolling to the top of a one-pixel
             element just below this row *)
          (row_height_px *. Float.of_int (index + 1)) +. header_offset
        in
        let y_px =
          if index <= range_start
          then Some to_top
          else if index >= range_end
          then Some to_bottom
          else None
        in
        y_px, to_top, to_bottom
    in
    let scroll_to_index =
      let%arr get_y_px_of_row_index
      and midpoint_of_container_x, _ = midpoint_of_container
      and table_body_selector in
      fun index ->
        let y_px, _, _ = get_y_px_of_row_index index in
        match y_px with
        | Some y_px ->
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling to index %{index#Int} at %{y_px#Float}0px"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container_x
            ~y_px
            ~selector:table_body_selector
            `Minimal
          |> Effect.ignore_m
        | None ->
          print_in_tests (fun () -> "skipping scroll because target already in view")
    in
    let leaves = Bonsai.map ~f:Header_tree.leaves headers in
    let scroll_to_column =
      let width (column : Column_size.t) =
        match column with
        | Visible { width_px } -> width_px
        | Hidden { prev_width_px = _ } -> 0.0
      in
      let get_offset_and_width =
        let%arr column_widths_for_reporting
          (* It's crucial that we use [column_widths_for_reporting] because it's set in
             both [resize_column_widths_to_fit] AND in the non-resizing version.
             [resize_column_widths_to_fit] does _not_ set [column_widths] whenever the
             width of the columns changes based on its content, only when the user
             manually resizes the columns
          *)
        and leaves in
        fun column_id ->
          List.fold_until
            ~init:0.0
            leaves
            ~finish:(fun _ -> None)
            ~f:(fun offset leaf ->
              let column_width =
                Option.map (Map.find column_widths_for_reporting leaf.column_id) ~f:width
                |> Option.value ~default:0.0
              in
              match
                Comparable.equal
                  (Comparator.compare Column_cmp.comparator)
                  leaf.column_id
                  column_id
              with
              | true -> Stop (Some (offset, column_width))
              | false -> Continue (offset +. column_width))
      in
      let%arr get_offset_and_width
      and get_y_px_of_row_index
      and table_body_visible_rect
      and table_body_selector in
      fun ~row_index column_id ->
        match table_body_visible_rect with
        | None -> Effect.Ignore
        | Some rect ->
          let offset_and_width = get_offset_and_width column_id in
          (match offset_and_width with
           | None -> Effect.Ignore
           | Some (offset, width) ->
             let y_px, to_top, _to_bottom = get_y_px_of_row_index row_index in
             let scroll_me ~y_px ~x_px =
               Scroll.to_position_inside_element
                 ~x_px
                 ~y_px
                 ~selector:table_body_selector
                 `Minimal
               |> Effect.ignore_m
             in
             let%bind.Effect () =
               print_in_tests (fun () ->
                 let column_id =
                   [%sexp (column_id : Column_cmp.t)] |> Sexp.to_string_hum
                 in
                 [%string
                   "scrolling cell at row index %{row_index#Int} and column id \
                    %{column_id} into view, if necessary"])
             in
             let column_start = offset in
             let column_end = offset +. width in
             let screen_left_bound = rect.min_x in
             let screen_right_bound = rect.max_x in
             if Float.(column_start < screen_left_bound)
             then (
               let y_px = Option.value y_px ~default:to_top in
               scroll_me ~y_px ~x_px:column_start)
             else if Float.(column_end > screen_right_bound)
             then (
               let y_px = Option.value y_px ~default:to_top in
               scroll_me ~y_px ~x_px:column_end)
             else (
               match y_px with
               | None -> Effect.Ignore
               | Some y_px ->
                 (* We know that column start is not to the left of the left bound, and we
                    know that column end is within the right bound as well, which means
                    that column start is within the visible bounds. This will not scroll
                    vertically, which is the desired behavior *)
                 scroll_me ~y_px ~x_px:column_start))
    in
    let scroll_to =
      let%arr scroll_to_column and scroll_to_index in
      function
      | `Cell (row_index, column) -> scroll_to_column ~row_index column
      | `Row row_index -> scroll_to_index row_index
    in
    let keep_top_row_in_position =
      let%arr range_without_preload
      and header_height_px
      and midpoint_of_container_x, _ = midpoint_of_container
      and table_body_selector
      and table_body_visible_rect in
      fun (`Px old_row_height_px) (`Px new_row_height_px) ->
        match range_without_preload (`Px old_row_height_px), table_body_visible_rect with
        | None, None | None, Some _ ->
          (* If there are no rows visible [range_without_preload] will return [None]. In
             this case we should do nothing because the scroll position is outside the
             tables jurisdiction. *)
          Effect.Ignore
        | Some _, None ->
          (* [range_without_preload] is expected to return a range only if the table body
             is partly visible, so this case is unexpected. We don't do anything except
             print because we need to visible rect to do anything correct. *)
          Effect.print_s
            [%message
              [%here]
                "BUG: the visible rect shouldn't be none when there is range of rows"]
        | Some (range_start, _range_end), Some table_body_visible_rect ->
          let old_row_height_px, new_row_height_px =
            Float.of_int old_row_height_px, Float.of_int new_row_height_px
          in
          (* If some rows of the table are visible, we scroll such that the top visible
             row remains in the same position in the viewport. *)
          let old_y_px = old_row_height_px *. Float.of_int range_start in
          let new_y_px = new_row_height_px *. Float.of_int range_start in
          let y_px =
            match Float.(new_y_px < old_y_px) with
            | true -> new_y_px -. header_height_px
            | false ->
              new_y_px
              -. header_height_px
              +. (table_body_visible_rect.max_y -. table_body_visible_rect.min_y)
              -. 1.0
          in
          let%bind.Effect () =
            print_in_tests (fun () ->
              [%string "scrolling position %{y_px#Float}px into view"])
          in
          Scroll.to_position_inside_element
            ~x_px:midpoint_of_container_x
            ~y_px
            ~selector:table_body_selector
            `Minimal
          |> Effect.ignore_m
    in
    let%sub () =
      (* If [row_height] changes, we want scrolling to follow the visible set of rows to
         their new location. To do this, we calculate the new position of the current top
         row, and then scroll their immediately. *)
      let callback =
        let%arr keep_top_row_in_position in
        fun prev_row_height new_row_height ->
          match prev_row_height with
          | Some prev_row_height ->
            keep_top_row_in_position prev_row_height new_row_height
          | None -> Effect.Ignore
      in
      Bonsai.Edge.on_change'
        ~trigger:`After_display
        ~sexp_of_model:[%sexp_of: Row_height_model.t]
        ~equal:[%equal: Row_height_model.t]
        row_height
        ~callback
        graph;
      Bonsai.return ()
    in
    let range_without_preload =
      let prev_row_height =
        Bonsai.previous_value
          ~sexp_of_model:[%sexp_of: Row_height_model.t]
          ~equal:[%equal: Row_height_model.t]
          row_height
          graph
      in
      let%arr prev_row_height and row_height and range_without_preload in
      (* If the [row_height] just changed, then we will be scrolling to a new position
         that will leave [range_without_preload] unchanged. Thus, we should intentionally
         _not_ account for the new change in row_height yet, since otherwise we will get
         flickering in the UI. *)
      let range =
        match prev_row_height with
        | Some prev_row_height -> range_without_preload prev_row_height
        | None -> range_without_preload row_height
      in
      Option.value range ~default:(0, 1)
    in
    let%sub { focus; visually_focused } =
      Focus.component
        focus_kind
        key_comparator
        column_id
        ~leaves
        ~collated
        ~range:range_without_preload
        ~scroll_to
        graph
    in
    let on_cell_click = Focus.get_on_cell_click focus_kind focus in
    let%sub body, body_for_testing =
      Table_body.component
        ~themed_attrs
        ~resize_column_widths_to_fit
        ~key_comparator
        ~column_id_comparator:column_id
        ~row_height
        ~headers
        ~leaves
        ~assoc
        ~column_widths
        ~visually_focused
        ~on_cell_click
        ~extra_row_attrs
        collated
        input_map
        graph
    in
    let head =
      Table_header.component
        headers
        ~column_id_equal:(Comparable.equal (Comparator.compare Column_cmp.comparator))
        ~focused_column:(Focus.get_focused_column focus_kind focus)
        ~themed_attrs
        ~resize_column_widths_to_fit
        ~column_widths
        ~set_column_width
        ~set_column_width_for_reporting
        ~set_header_client_rect
        graph
    in
    let view =
      let vis_change_attr =
        let%arr set_table_body_visible_rect and set_table_body_client_rect in
        Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:(fun bounds -> set_table_body_client_rect (Some bounds))
          ~visible_rect_changed:(fun visible_bounds ->
            set_table_body_visible_rect visible_bounds)
      in
      let rows_height =
        let%arr row_count
        and (`Px row_height_px) = row_height in
        row_count * row_height_px
      in
      let%arr head
      and body
      and private_body_classname
      and vis_change_attr
      and header_height_px
      and rows_height
      and resize_column_widths_to_fit
      and themed_attrs in
      Table_view.Table.view
        themed_attrs
        ~private_body_classname
        ~vis_change_attr
        ~header_height:header_height_px
        ~rows_height
        ~resize_column_widths_to_fit
        head
        body
    in
    let range =
      let%arr low, high = range_without_preload
      and row_count in
      let low = Int.max 0 (low - preload_rows) in
      let low =
        (* always fetch a range starting at an even index in order to make css-selecting
           on even and odd rows work. *)
        low - (low % 2)
      in
      let high = Int.min row_count (high + preload_rows) in
      let low, high = low, Int.max low high in
      low, high
    in
    let column_widths =
      let%arr column_widths_for_reporting in
      lazy
        (Map.to_alist column_widths_for_reporting
         |> List.filter_map ~f:(function
           | column_id, Visible { width_px }
           | column_id, Hidden { prev_width_px = Some width_px } ->
             Some (column_id, `Px_float width_px)
           | _, Hidden { prev_width_px = None } -> None))
    in
    let%arr view
    and range
    and body_for_testing
    and focus
    and set_column_width
    and column_widths in
    let for_testing =
      let%map.Lazy body = body_for_testing in
      { For_testing.body }
    in
    { Result.view; range; for_testing; focus; set_column_width; column_widths }
  ;;

  let component
    (type column_id key data cmp)
    ?(styling = Which_styling.From_theme)
    ?(resize_column_widths_to_fit = Bonsai.return false)
    ?round_column_width
    ?(preload_rows = default_preload)
    ?extra_row_attrs
    (key_comparator : (key, cmp) Comparator.Module.t)
    ~focus
    ~row_height
    ~(columns : (key, data, column_id) Column_intf.t)
    (collated : (key, data) Collated.t Bonsai.t)
    (local_ graph)
    =
    implementation
      ?extra_row_attrs
      ~styling
      ~resize_column_widths_to_fit
      ?round_column_width
      ~preload_rows
      ~wrap_header:(return (fun ~column_id:_ view -> view))
      key_comparator
      ~focus
      ~row_height
      ~columns
      collated
      graph
  ;;

  let collate
    (type k v cmp filter order)
    ?operation_order
    ~filter_equal
    ~order_equal
    ~(filter_to_predicate : filter -> _)
    ~(order_to_compare : order -> _)
    (data : (k, v, cmp) Map.t Bonsai.t)
    (collate : (k, filter, order) Collate_params.t Bonsai.t)
    (local_ graph)
    =
    let data_and_collate = Bonsai.both data collate in
    let%sub collated, key_rank =
      Bonsai.Incr.compute
        data_and_collate
        ~f:(fun data_and_collate ->
          let open Ui_incr.Let_syntax in
          let%pattern_bind data, collate = data_and_collate in
          let collate_result =
            Incr_map_collate.collate
              ?operation_order
              ~filter_equal
              ~order_equal
              ~filter_to_predicate
              ~order_to_compare
              data
              collate
          in
          Ui_incr.both
            (Incr_map_collate.collated collate_result)
            (Ui_incr.map (Incr_map_collate.key_rank collate_result) ~f:Effect.of_sync_fun))
        graph
    in
    collated, key_rank
  ;;
end

module Basic = struct
  module Focus = struct
    include Focus

    type ('a, 'p, 'k, 'c) t =
      | None : (unit, unit, 'k, 'c) t
      | By_row :
          { on_change : ('k option -> unit Effect.t) Bonsai.t }
          -> ('k Focus_by_row.optional, 'k option, 'k, 'c) t
      | By_cell :
          { on_change : (('k * 'c) option -> unit Effect.t) Bonsai.t }
          -> (('k, 'c) By_cell.optional, ('k * 'c) option, 'k, 'c) t
  end

  module Result = struct
    type ('focus, 'key, 'column_id) t =
      { view : Vdom.Node.t
      ; for_testing : For_testing.t Lazy.t
      ; focus : 'focus
      ; num_filtered_rows : int
      ; sortable_state : 'column_id Sortable.t
      ; set_column_width : column_id:'column_id -> [ `Px_float of float ] -> unit Effect.t
      ; column_widths : ('column_id * [ `Px_float of float ]) list lazy_t
      ; key_rank : 'key -> int option Effect.t
      }
    [@@deriving fields ~getters]
  end

  module Columns = struct
    type ('key, 'data, 'column_id) t = ('key, 'data, 'column_id) Column_intf.with_sorter

    include Columns.Basic
    module Indexed_column_id = Indexed_column_id
    module Dynamic_cells = Old_columns.Dynamic_cells_with_sorter
    module Dynamic_columns = Old_columns.Dynamic_columns_with_sorter
    module Dynamic_experimental = Old_columns.Dynamic_experimental_with_sorter
  end

  module Rank_range = struct
    type t = int Collate_params.Which_range.t [@@deriving sexp, equal]
  end

  let component
    (type key presence focus data cmp column_id)
    ?(styling = Which_styling.From_theme)
    ?(resize_column_widths_to_fit = Bonsai.return false)
    ?round_column_width
    ?filter
    ?override_sort
    ?default_sort
    ?(wrap_header = Bonsai.return (Sortable.Wrap_header.clickable_with_icon ()))
    ?(preload_rows = default_preload)
    ?extra_row_attrs
    (key_comparator : (key, cmp) Comparator.Module.t)
    ~(focus : (focus, presence, key, column_id) Focus.t)
    ~row_height
    ~(columns : (key, data, column_id) Column_intf.with_sorter)
    map
    (local_ graph)
    =
    let module Key_cmp = (val key_comparator) in
    let filter = Bonsai.transpose_opt filter in
    let rank_range, set_rank_range =
      Bonsai.state
        (Collate_params.Which_range.To 0)
        ~sexp_of_model:[%sexp_of: Rank_range.t]
        ~equal:[%equal: Rank_range.t]
        graph
    in
    let (Column_intf.Y { value; vtable; column_id }) = columns in
    let module Col_id = (val column_id) in
    let sortable_state =
      Sortable.state
        ~equal:(Comparable.equal (Comparator.compare Col_id.comparator))
        ()
        graph
    in
    let module Column = (val vtable) in
    let default_sort =
      match default_sort with
      | None -> Bonsai.return None
      | Some v -> v >>| Option.some
    in
    let sorters = Column.sorters value graph in
    let wrap_header =
      let%arr wrap_header and sorters and sortable_state in
      let is_sortable column_id = Map.mem sorters column_id in
      wrap_header sortable_state ~is_sortable
    in
    let collate =
      let override_sort =
        match override_sort with
        | None -> Bonsai.return None
        | Some override -> override >>| Option.some
      in
      let order =
        let%arr sorters and default_sort and sortable_state and override_sort in
        let override_sort =
          Option.map override_sort ~f:(fun override_sort ->
            override_sort (Comparator.compare Key_cmp.comparator))
        in
        Order.to_compare
          (Sortable.order sortable_state)
          ?override_sort
          ~sorters
          ~default_sort
      in
      let%arr filter and order and rank_range in
      let key_range = Collate_params.Which_range.All_rows in
      { Collate_params.Stable.V1.filter; order; key_range; rank_range }
      |> Collate_params.of_stable_v1
    in
    let collated, key_rank =
      Expert.collate
        ~filter_equal:phys_equal
        ~filter_to_predicate:Fn.id
        ~order_equal:phys_equal
        ~order_to_compare:Fn.id
        map
        collate
        graph
    in
    let focus : (focus, presence, key, column_id) Expert.Focus.Kind.t =
      match focus with
      | None -> None
      | By_row { on_change } ->
        let compute_presence focus (local_ _graph) =
          let%arr focus and map in
          match focus with
          | None -> None
          | Some focus -> if Map.mem map focus then Some focus else None
        in
        By_row { on_change; compute_presence; key_rank }
      | By_cell { on_change } ->
        let compute_presence focus (local_ _graph) =
          let%arr focus and map in
          match focus with
          | None -> None
          | Some ((focused_key, _) as focus) ->
            if Map.mem map focused_key then Some focus else None
        in
        By_cell { on_change; compute_presence; key_rank }
    in
    let num_filtered_rows =
      let%arr collated in
      Collated.num_filtered_rows collated
    in
    let columns = Column_intf.T { value; vtable = (module Column); column_id } in
    let%sub ({ range = viewed_range; _ } as result) =
      let x =
        Expert.implementation
          ~styling
          ~resize_column_widths_to_fit
          ~preload_rows
          ?round_column_width
          ?extra_row_attrs
          ~wrap_header
          key_comparator
          ~focus
          ~row_height
          ~columns
          collated
          graph
      in
      x
    in
    let () =
      Bonsai.Edge.on_change
        ~trigger:`After_display
        ~sexp_of_model:[%sexp_of: int * int]
        ~equal:[%equal: int * int]
        viewed_range
        ~callback:
          (let%map set_rank_range in
           fun (low, high) ->
             set_rank_range (Collate_params.Which_range.Between (low, high)))
        graph
    in
    let%arr { view; for_testing; range = _; focus; set_column_width; column_widths } =
      result
    and num_filtered_rows
    and sortable_state
    and key_rank in
    { Result.view
    ; for_testing
    ; focus
    ; num_filtered_rows
    ; sortable_state
    ; set_column_width
    ; column_widths
    ; key_rank
    }
  ;;
end
