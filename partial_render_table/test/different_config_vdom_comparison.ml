open! Core
open! Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax
open Bonsai_web_ui_partial_render_table_configs_for_testing

let get_view ~resize_column_widths_to_fit ~num_rows config =
  let input =
    Sharable.Input.create
      ~resize_column_widths_to_fit
      (Symbol_table.Row.init_rows num_rows)
  in
  let component = All_apis_configs.computation config (return input) in
  let handle =
    Handle.create
      (Result_spec.vdom (fun { All_apis_configs.Prt_output.view; _ } -> view))
      component
  in
  Handle.show_into_string handle
;;

let assert_all_same ~num_rows ~resize_column_widths_to_fit configs =
  let first = ref None in
  let ok = ref true in
  List.iter configs ~f:(fun config ->
    let view = get_view ~resize_column_widths_to_fit ~num_rows config in
    match !first with
    | None -> first := Some (config, view)
    | Some (first_config, first_view) ->
      if not (String.equal first_view view)
      then (
        ok := false;
        print_endline
          [%string
            "Config %{All_apis_configs.name config} doesn't match first run \
             (%{All_apis_configs.name first_config})!:"];
        Expect_test_patdiff.print_patdiff ~context:4 first_view view));
  if !ok then print_endline "Ok!"
;;

let test ~num_rows configs =
  print_endline "===== Not Autosize =====";
  assert_all_same ~num_rows ~resize_column_widths_to_fit:false configs;
  print_endline "\n\n===== Autosize =====";
  assert_all_same ~num_rows ~resize_column_widths_to_fit:true configs
;;

let duplicate_col_partition =
  List.partition_tf ~f:(function
    | All_apis_configs.New_api { duplicate_col; _ } -> duplicate_col
    | Dynamic_experimental _ -> false
    | Dynamic_cols { duplicate_col; _ } -> duplicate_col
    | Dynamic_cells { duplicate_col; _ } -> duplicate_col)
;;

let group_vs_flat_partition =
  List.partition_tf ~f:(function
    | All_apis_configs.New_api { col_groups; _ } -> col_groups
    | Dynamic_experimental _ -> false
    | Dynamic_cols { col_groups; _ } -> col_groups
    | Dynamic_cells { col_groups; _ } -> col_groups)
;;

let counters_partition =
  List.partition_tf ~f:(function
    | All_apis_configs.New_api { counters_in_cells; _ } -> counters_in_cells
    | Dynamic_experimental { counters_in_cells; _ } -> counters_in_cells
    | Dynamic_cells { counters_in_cells; _ } -> counters_in_cells
    | Dynamic_cols { which_dynamic_cols = Counters; _ } -> true
    | Dynamic_cols { which_dynamic_cols = No_counters | No_counters_constant_foldable; _ }
      -> false)
;;

let is_dyn_cells_partition =
  List.partition_tf ~f:(function
    | All_apis_configs.Dynamic_cells _ -> true
    | Dynamic_cols _ | Dynamic_experimental _ -> false
    | New_api { cols = Static; _ } -> false
    | New_api { cols = Dynamic | Dynamic_constant_foldable; _ } -> false)
;;

let%expect_test "Same Structure" =
  let f all =
    let all_grouped, all_flat = group_vs_flat_partition all in
    let flat_counters, flat_no_counters = counters_partition all_flat in
    let grouped_counters, grouped_no_counters = counters_partition all_grouped in
    let grouped_counters_static, grouped_counters_dynamic =
      is_dyn_cells_partition grouped_counters
    in
    let grouped_no_counters_static, grouped_no_counters_dynamic =
      is_dyn_cells_partition grouped_no_counters
    in
    List.iter
      [ flat_counters
      ; flat_no_counters
      ; grouped_counters_static
      ; grouped_counters_dynamic
      ; grouped_no_counters_static
      ; grouped_no_counters_dynamic
      ]
      ~f:(fun should_be_same ->
        test ~num_rows:10 should_be_same;
        [%expect
          {|
          ===== Not Autosize =====
          Ok!


          ===== Autosize =====
          Ok!
          |}])
  in
  let duplicate_cols, not_duplicate_cols = duplicate_col_partition All_apis_configs.all in
  f duplicate_cols;
  f not_duplicate_cols
;;

let%expect_test "dyn cells with `visible`" =
  test
    ~num_rows:1
    [ New_api
        { cols = Static
        ; col_groups = true
        ; render_cell_kind = Pure
        ; counters_in_cells = true
        ; duplicate_col = false
        }
    ; Dynamic_cells { col_groups = true; counters_in_cells = true; duplicate_col = false }
    ];
  [%expect
    {|
    ===== Not Autosize =====
    Config dyn cells (counters) (groups) doesn't match first run (new pure (static cols) (counters) (groups))!:
    === DIFF HUNK ===
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </td>
    +|        <td colspan="1"
    +|            class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    +|            size_tracker=<fun>
    +|            style={
    +|              width: 50px;
    +|              display: none;
    +|            }> max_edge </td>
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
    === DIFF HUNK ===
                     }>
                  0.
                  <button @on_click> 0 </button>
                </div>
    +|          <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
    +|               @on_click
    +|               style={
    +|                 height: 30px;
    +|                 min-height: 30px;
    +|                 max-height: 30px;
    +|                 width: 0.00px;
    +|                 min-width: 0.00px;
    +|                 max-width: 0.00px;
    +|                 display: none;
    +|               }> </div>
                <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
                     @on_click
                     style={
                       height: 30px;


    ===== Autosize =====
    Config dyn cells (counters) (groups) doesn't match first run (new pure (static cols) (counters) (groups))!:
    === DIFF HUNK ===
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> edge </th>
    +|        <th colspan="1"
    +|            class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    +|            size_tracker=<fun>
    +|            style={
    +|              width: 50px;
    +|              display: none;
    +|            }> max_edge </th>
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
                  size_tracker=<fun>
                  style={
    === DIFF HUNK ===
                    <button @on_click> 0 </button>
                  </div>
                </div>
              </div>
    +|        <div class="autosize_table_cell_wrapper_hash_replaced_in_test table_view__inline_class_hash_replaced_in_test">
    +|          <div class="table_view__inline_class_hash_replaced_in_test"
    +|               style={
    +|                 display: none;
    +|                 height: 30px;
    +|                 min-height: 30px;
    +|                 max-height: 30px;
    +|               }>
    +|            <div class="autosize_wrapped_cell_hash_replaced_in_test body_cell_hash_replaced_in_test"
    +|                 @on_click
    +|                 style={
    +|                   height: 30px;
    +|                   min-height: 30px;
    +|                   max-height: 30px;
    +|                 }> </div>
    +|          </div>
    +|        </div>
              <div class="autosize_table_cell_wrapper_hash_replaced_in_test table_view__inline_class_hash_replaced_in_test">
                <div class="table_view__inline_class_hash_replaced_in_test"
                     style={
                       height: 30px;
    |}]
;;

let%expect_test "duplicate col adds the another col, with the same view" =
  test
    ~num_rows:1
    [ New_api
        { cols = Static
        ; col_groups = true
        ; render_cell_kind = Pure
        ; counters_in_cells = true
        ; duplicate_col = false
        }
    ; New_api
        { cols = Static
        ; col_groups = true
        ; render_cell_kind = Pure
        ; counters_in_cells = true
        ; duplicate_col = true
        }
    ];
  [%expect
    {|
    ===== Not Autosize =====
    Config new pure (static cols) (counters) (groups) doesn't match first run (new pure (static cols) (counters) (groups))!:
    === DIFF HUNK ===
              <td class="header_cell_hash_replaced_in_test"> </td>
              <td class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Edge </td>
              <td colspan="4"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Book </td>
    +|        <td class="header_cell_hash_replaced_in_test"> </td>
            </tr>
            <tr class="header_row_hash_replaced_in_test">
              <td colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    === DIFF HUNK ===
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> asize </td>
    +|        <td colspan="1"
    +|            class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    +|            size_tracker=<fun>
    +|            style={
    +|              width: 50px;
    +|            }> edge </td>
            </tr>
          </tbody>
        </table>
        <div class="default_partial_render_table_body_hash_replaced_in_test partial-render-table-body-bonsai_path_replaced_in_test"
    === DIFF HUNK ===
                     }>
                  0
                  <button @on_click> 0 </button>
                </div>
    +|          <div class="body_cell_hash_replaced_in_test cell_hash_replaced_in_test"
    +|               @on_click
    +|               style={
    +|                 height: 30px;
    +|                 min-height: 30px;
    +|                 max-height: 30px;
    +|                 width: 0.00px;
    +|                 min-width: 0.00px;
    +|                 max-width: 0.00px;
    +|               }>
    +|            0.
    +|            <button @on_click> 0 </button>
    +|          </div>
              </div>
            </div>
          </div>
        </div>


    ===== Autosize =====
    Config new pure (static cols) (counters) (groups) doesn't match first run (new pure (static cols) (counters) (groups))!:
    === DIFF HUNK ===
              <th class="header_cell_hash_replaced_in_test"> </th>
              <th class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Edge </th>
              <th colspan="4"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test"> Book </th>
    +|        <th class="header_cell_hash_replaced_in_test"> </th>
            </tr>
            <tr class="header_row_hash_replaced_in_test">
              <th colspan="1"
                  class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    === DIFF HUNK ===
                  size_tracker=<fun>
                  style={
                    width: 50px;
                  }> asize </th>
    +|        <th colspan="1"
    +|            class="header_cell_hash_replaced_in_test header_label_hash_replaced_in_test leaf_header_hash_replaced_in_test leaf_header_resizable_hash_replaced_in_test"
    +|            size_tracker=<fun>
    +|            style={
    +|              width: 50px;
    +|            }> edge </th>
            </tr>
          </thead>
          <div style={ display: table-row-group; position: relative; }>
            <div @key=top_padding style={ height: 0px; }> </div>
    === DIFF HUNK ===
                    <button @on_click> 0 </button>
                  </div>
                </div>
              </div>
    +|        <div class="autosize_table_cell_wrapper_hash_replaced_in_test table_view__inline_class_hash_replaced_in_test">
    +|          <div class="table_view__inline_class_hash_replaced_in_test"
    +|               style={
    +|                 height: 30px;
    +|                 min-height: 30px;
    +|                 max-height: 30px;
    +|               }>
    +|            <div class="autosize_wrapped_cell_hash_replaced_in_test body_cell_hash_replaced_in_test"
    +|                 @on_click
    +|                 style={
    +|                   height: 30px;
    +|                   min-height: 30px;
    +|                   max-height: 30px;
    +|                 }>
    +|              0.
    +|              <button @on_click> 0 </button>
    +|            </div>
    +|          </div>
    +|        </div>
            </div>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          </div>
        </div>
    |}]
;;

let%expect_test "col dependency configs" =
  let input =
    { Col_dependency_configs.Input.col_dependency = 0
    ; data = Symbol_table.Row.init_rows 1
    }
  in
  let get_view config =
    let component = (Col_dependency_configs.computation config) (return input) in
    let handle = Handle.create (Result_spec.vdom Fn.id) component in
    Handle.show_into_string handle
  in
  let assert_all_same ~which_column =
    let first = ref None in
    let ok = ref true in
    List.iter
      [ { Col_dependency_configs.which_column; which_api = Dynamic_cells }
      ; { Col_dependency_configs.which_column; which_api = Dynamic_cols }
      ]
      ~f:(fun config ->
        let view = get_view config in
        match !first with
        | None -> first := Some (config, view)
        | Some (first_config, first_view) ->
          if not (String.equal first_view view)
          then (
            ok := false;
            print_endline
              [%string
                "Config %{Col_dependency_configs.name config} doesn't match first run \
                 (%{Col_dependency_configs.name first_config})!:"];
            Expect_test_patdiff.print_patdiff ~context:4 first_view view));
    if !ok then print_endline "Ok!"
  in
  assert_all_same ~which_column:First;
  [%expect {| Ok! |}];
  assert_all_same ~which_column:Middle;
  [%expect {| Ok! |}];
  assert_all_same ~which_column:Last;
  [%expect {| Ok! |}]
;;
