open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "new - old"

(* This test compares the old vs new API for dynamic cells and columns.
   Ideally, we'd like these numbers to be negative or zero, since that means the new API
   requires fewer incremental nodes than the old one. *)

let test_startup configs =
  let startup_inputs =
    List.map [ 100; 100_000 ] ~f:(fun size ->
      Int.to_string size, Prt_input.create (Row.init_rows size))
  in
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    (module Config)
    startup_inputs
    configs
;;

let configs ~col_groups =
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
  let configs = configs ~col_groups:false in
  test_startup configs;
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
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
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
    │ Focus by key (key not present) and unfocus in 10 element map            │ .             │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .             │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .             │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .             │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .             │ +4 (8%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .             │ +4 (3%)         │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .             │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .             │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .             │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .             │ +4 (1%)         │ .            │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_608 (39%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_608 (39%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_608 (39%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_608 (39%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_608 (39%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -2_344 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -2_344 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -2_344 (43%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -11_624 (43%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -11_624 (43%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -23_224 (44%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -246 (65%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_406 (83%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_430 (83%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_430 (83%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_160 (50%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_160 (50%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_160 (50%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_160 (50%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -8_000 (57%)  │ .               │ .            │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -946 (44%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -636 (74%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -952 (44%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -642 (74%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -102 (68%)   │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -792 (25%)   │ .               │ .            │
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
  let configs = configs ~col_groups:true in
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (new - old) =======
    ┌─────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                         │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├─────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ dyn cells: 100          │ -15 (15%)  │ -4_868 (41%) │ -6_388 (45%) │ -6_387 (45%)  │
    │ dyn cols, no cf: 100    │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100       │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cells: 100000       │ -15 (15%)  │ -4_916 (41%) │ -6_451 (45%) │ -6_450 (45%)  │
    │ dyn cols, no cf: 100000 │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100000    │ -1 (1%)    │ .            │ .            │ .             │
    └─────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -548 (37%)   │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -4_868 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -4_916 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -4_916 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -4_916 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ -548 (37%)   │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ -4_868 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ -4_916 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -4_916 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -4_916 (41%) │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ -548 (37%)   │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ -4_868 (41%) │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ -4_916 (41%) │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ -4_916 (41%) │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ -4_916 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ -548 (37%)   │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ -4_868 (41%) │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ -4_916 (41%) │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ -4_916 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ -4_916 (41%) │ .               │ .            │
    │ Page up and down in 10 element map                                      │ -548 (37%)   │ .               │ .            │
    │ Page up and down in 100 element map                                     │ -4_868 (41%) │ .               │ .            │
    │ Page up and down in 101 element map                                     │ -4_916 (41%) │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ -4_916 (41%) │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ -4_916 (41%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -116 (25%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -548 (37%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -116 (25%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -548 (37%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -4_868 (41%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -548 (37%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -548 (37%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -548 (37%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_468 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_468 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -4_868 (41%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -548 (37%)   │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -4_868 (41%) │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -4_916 (41%) │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -4_916 (41%) │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -548 (37%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -548 (37%)   │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ -548 (37%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -548 (37%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -548 (37%)   │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -548 (37%)   │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -4_868 (41%) │ .               │ .            │
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
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -828 (28%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -828 (28%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -828 (28%)   │ .               │ .            │
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
    │ Focus by key (key not present) and unfocus in 10 element map            │ +3 (6%)       │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +4 (8%)       │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +4 (8%)       │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +4 (8%)       │ +4 (8%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +4 (8%)       │ +4 (8%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ +4 (3%)       │ +4 (3%)         │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ +4 (1%)       │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ +4 (1%)       │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +4 (1%)       │ +4 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +4 (1%)       │ +4 (1%)         │ .            │
    │ Focus up and down in 10 element map                                     │ +2 (3%)       │ +2 (3%)         │ .            │
    │ Focus up and down in 100 element map                                    │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus up and down in 101 element map                                    │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus up and down in 1000 element map                                   │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus up and down in 10000 element map                                  │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10 rows                              │ +2 (3%)       │ +2 (3%)         │ .            │
    │ Focus left and right in a map with 100 rows                             │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 101 rows                             │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 1000 rows                            │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Focus left and right in a map with 10000 rows                           │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Page up and down in 10 element map                                      │ +2 (3%)       │ +2 (3%)         │ .            │
    │ Page up and down in 100 element map                                     │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Page up and down in 101 element map                                     │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Page up and down in 1000 element map                                    │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Page up and down in 10000 element map                                   │ +2 (1%)       │ +2 (1%)         │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_448 (37%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_448 (37%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_448 (37%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_448 (37%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_448 (37%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_992 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_992 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_992 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -9_672 (42%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -9_672 (42%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -19_272 (42%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -284 (68%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_624 (84%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_650 (84%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_650 (84%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .             │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_027 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_215 (52%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_027 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_215 (52%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_027 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_215 (52%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_027 (48%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_215 (52%)  │ .               │ .            │
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
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -530 (70%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -788 (42%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -535 (70%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -85 (64%)    │ .               │ .            │
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
