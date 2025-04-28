open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "new - old"

(* This test compares the old vs new API for dynamic cells and columns.
   Ideally, we'd like these numbers to be negative or zero, since that means the new API
   requires fewer incremental nodes than the old one. *)

let test_startup pairs =
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    (Symbol_table.startup_inputs [ 100; 100_000 ])
;;

let pairs ~col_groups =
  [ ( "dyn cells"
    , Config.Dynamic_cells { counters_in_cells = true; col_groups; duplicate_col = false }
    , Config.New_api
        { counters_in_cells = true
        ; col_groups
        ; cols = Static
        ; render_cell_kind = Stateful_cells
        ; duplicate_col = false
        } )
  ; ( "dyn cols, no cf"
    , Dynamic_cols { which_dynamic_cols = No_counters; col_groups; duplicate_col = false }
    , New_api
        { counters_in_cells = false
        ; col_groups
        ; cols = Dynamic
        ; render_cell_kind = Pure
        ; duplicate_col = false
        } )
  ; ( "dyn cols, cf"
    , Dynamic_cols
        { which_dynamic_cols = No_counters_constant_foldable
        ; col_groups
        ; duplicate_col = false
        }
    , New_api
        { counters_in_cells = false
        ; col_groups
        ; cols = Dynamic_constant_foldable
        ; render_cell_kind = Pure
        ; duplicate_col = false
        } )
  ]
;;

let%expect_test "Old -> New, no col groups" =
  let pairs = pairs ~col_groups:false in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (new - old) =======
    ┌─────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                         │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├─────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ dyn cells: 100          │ -15 (15%)  │ -5_858 (43%) │ -7_670 (47%) │ -7_670 (47%)  │
    │ dyn cols, no cf: 100    │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100       │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cells: 100000       │ -15 (15%)  │ -5_916 (43%) │ -7_746 (47%) │ -7_746 (47%)  │
    │ dyn cols, no cf: 100000 │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100000    │ -1 (1%)    │ .            │ .            │ .             │
    └─────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -638 (38%)   │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -5_858 (43%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -5_916 (43%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -5_916 (43%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -5_916 (43%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ -638 (38%)   │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ -5_858 (43%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ -5_916 (43%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -5_916 (43%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -5_916 (43%) │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ -638 (38%)   │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ -5_858 (43%) │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ -5_916 (43%) │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ -5_916 (43%) │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ -5_916 (43%) │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ -638 (38%)   │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ -5_858 (43%) │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ -5_916 (43%) │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ -5_916 (43%) │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ -5_916 (43%) │ .               │ .            │
    │ Page up and down in 10 element map                                      │ -638 (38%)   │ .               │ .            │
    │ Page up and down in 100 element map                                     │ -5_858 (43%) │ .               │ .            │
    │ Page up and down in 101 element map                                     │ -5_916 (43%) │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ -5_916 (43%) │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ -5_916 (43%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -116 (25%)   │ +3 (1%)         │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -638 (38%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -116 (25%)   │ +3 (1%)         │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -638 (38%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -5_858 (43%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -638 (38%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -638 (38%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -638 (38%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_958 (42%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_958 (42%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -5_858 (43%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -638 (38%)   │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -5_858 (43%) │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -5_916 (43%) │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -5_916 (43%) │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -638 (38%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -638 (38%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -638 (38%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -638 (38%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -638 (38%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -638 (38%)   │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -5_858 (43%) │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells     │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .             │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .             │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .             │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .             │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .             │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .             │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .             │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .             │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .             │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .             │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .             │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .             │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .             │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .             │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .             │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .             │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .             │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .             │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .             │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .             │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -448 (30%)    │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_216 (47%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -448 (30%)    │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_216 (47%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_216 (47%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_008 (29%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_008 (29%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_008 (29%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -5_488 (30%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -5_488 (30%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -11_088 (31%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .             │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .             │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .             │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .             │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .             │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .             │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .             │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .             │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .             │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells     │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -3 (2%)       │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -2 (1%)       │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -2 (1%)       │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -2 (1%)       │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -2 (1%)       │ +6 (4%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .             │ +6 (3%)         │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .             │ +6 (2%)         │ .            │
    │ Focus up and down in 10 element map                                     │ .             │ +2 (3%)         │ .            │
    │ Focus up and down in 100 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 101 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 1000 element map                                   │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 10000 element map                                  │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10 rows                              │ .             │ +2 (3%)         │ .            │
    │ Focus left and right in a map with 100 rows                             │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 101 rows                             │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 10 element map                                      │ .             │ +2 (3%)         │ .            │
    │ Page up and down in 100 element map                                     │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 101 element map                                     │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 1000 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 10000 element map                                   │ .             │ +2 (1%)         │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_610 (37%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_610 (37%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_610 (37%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_610 (37%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_610 (37%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -2_346 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -2_346 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -2_346 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -11_626 (44%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -11_626 (44%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -23_226 (44%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -246 (64%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_406 (83%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_430 (83%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_430 (83%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_162 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_602 (53%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_162 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_602 (53%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_162 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_602 (53%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_162 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_602 (53%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -8_002 (57%)  │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -946 (43%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -636 (72%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -952 (44%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -642 (72%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -102 (58%)   │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -792 (24%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -792 (24%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -792 (24%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -8_712 (26%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .            │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘
    |}]
;;

let%expect_test "Old -> New, col groups" =
  let pairs = pairs ~col_groups:true in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (new - old) =======
    ┌─────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                         │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├─────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ dyn cells: 100          │ -15 (15%)  │ -4_864 (41%) │ -6_384 (45%) │ -6_383 (45%)  │
    │ dyn cols, no cf: 100    │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100       │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cells: 100000       │ -15 (15%)  │ -4_912 (41%) │ -6_447 (45%) │ -6_446 (45%)  │
    │ dyn cols, no cf: 100000 │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100000    │ -1 (1%)    │ .            │ .            │ .             │
    └─────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -544 (36%)   │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -4_864 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -4_912 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -4_912 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -4_912 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ -544 (36%)   │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ -4_864 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ -4_912 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -4_912 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -4_912 (41%) │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ -544 (36%)   │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ -4_864 (41%) │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ -4_912 (41%) │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ -4_912 (41%) │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ -4_912 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ -544 (36%)   │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ -4_864 (41%) │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ -4_912 (41%) │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ -4_912 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ -4_912 (41%) │ .               │ .            │
    │ Page up and down in 10 element map                                      │ -544 (36%)   │ .               │ .            │
    │ Page up and down in 100 element map                                     │ -4_864 (41%) │ .               │ .            │
    │ Page up and down in 101 element map                                     │ -4_912 (41%) │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ -4_912 (41%) │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ -4_912 (41%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -112 (24%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -544 (36%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -112 (24%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -544 (36%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -4_864 (41%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -544 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -544 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -544 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_464 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_464 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -4_864 (41%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -544 (36%)   │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -4_864 (41%) │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -4_912 (41%) │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -4_912 (41%) │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -544 (36%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -544 (36%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -544 (36%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -544 (36%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -544 (36%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -544 (36%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -544 (37%)   │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -4_864 (41%) │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -368 (29%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_008 (45%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -368 (29%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_008 (45%) │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_008 (45%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -828 (27%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -828 (27%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -828 (27%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -4_508 (29%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -4_508 (29%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -9_108 (29%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .            │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells     │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .             │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .             │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .             │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .             │ +6 (4%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .             │ +6 (4%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .             │ +6 (3%)         │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .             │ +6 (2%)         │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .             │ +6 (2%)         │ .            │
    │ Focus up and down in 10 element map                                     │ .             │ +2 (3%)         │ .            │
    │ Focus up and down in 100 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 101 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 1000 element map                                   │ .             │ +2 (1%)         │ .            │
    │ Focus up and down in 10000 element map                                  │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10 rows                              │ .             │ +2 (3%)         │ .            │
    │ Focus left and right in a map with 100 rows                             │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 101 rows                             │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .             │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 10 element map                                      │ .             │ +2 (3%)         │ .            │
    │ Page up and down in 100 element map                                     │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 101 element map                                     │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 1000 element map                                    │ .             │ +2 (1%)         │ .            │
    │ Page up and down in 10000 element map                                   │ .             │ +2 (1%)         │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_448 (36%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_448 (36%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_448 (36%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_448 (36%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_448 (36%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_988 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_988 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_988 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -9_668 (42%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -9_668 (42%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -19_268 (42%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -274 (67%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_524 (84%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_549 (84%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_549 (84%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_027 (46%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_215 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_027 (46%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_215 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_027 (46%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_215 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_027 (46%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_215 (51%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -6_670 (54%)  │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -783 (42%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -530 (68%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -788 (42%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -535 (68%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -85 (54%)    │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -7_128 (25%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .            │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘
    |}]
;;
