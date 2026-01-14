open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "foldable - not foldable"

(* This test compares "full-power" vs constant-foldable structures. We expect the constant
   foldable ones to be significantly smaller, so these numbers should all be negative. *)

let test_startup pairs =
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    (Symbol_table.startup_inputs [ 100; 100_000 ])
;;

let%expect_test "Constant folding cols" =
  let pairs =
    [ ( "new api, counters"
      , Config.New_api
          { counters_in_cells = true
          ; cols = Dynamic
          ; col_groups = false
          ; render_cell_kind = Stateful_cells
          ; duplicate_col = false
          }
      , Config.New_api
          { counters_in_cells = true
          ; cols = Dynamic_constant_foldable
          ; col_groups = false
          ; render_cell_kind = Stateful_cells
          ; duplicate_col = false
          } )
    ; ( "new api, no counters"
      , Config.New_api
          { counters_in_cells = false
          ; cols = Dynamic
          ; col_groups = false
          ; render_cell_kind = Stateful_cells
          ; duplicate_col = false
          }
      , Config.New_api
          { counters_in_cells = false
          ; cols = Dynamic_constant_foldable
          ; col_groups = false
          ; render_cell_kind = Stateful_cells
          ; duplicate_col = false
          } )
    ; ( "dyn cols, counters"
      , Dynamic_cols
          { which_dynamic_cols = No_counters; col_groups = false; duplicate_col = false }
      , Dynamic_cols
          { which_dynamic_cols = No_counters_constant_foldable
          ; col_groups = false
          ; duplicate_col = false
          } )
    ]
  in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (foldable - not foldable) =======
    ┌──────────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                              │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├──────────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ new api, counters: 100       │ +1 (1%)    │ -305 (4%)    │ -2_606 (23%) │ -2_605 (23%)  │
    │ new api, no counters: 100    │ -13 (16%)  │ -3_008 (76%) │ -7_009 (83%) │ -7_010 (83%)  │
    │ dyn cols, counters: 100      │ .          │ .            │ .            │ .             │
    │ new api, counters: 100000    │ +1 (1%)    │ -308 (4%)    │ -2_632 (23%) │ -2_631 (23%)  │
    │ new api, no counters: 100000 │ -13 (16%)  │ -3_038 (76%) │ -7_079 (83%) │ -7_080 (83%)  │
    │ dyn cols, counters: 100000   │ .          │ .            │ .            │ .             │
    └──────────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (foldable - not foldable) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                                          │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map                                │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus up and down in 10 element map                                                      │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Focus up and down in 100 element map                                                     │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Focus up and down in 101 element map                                                     │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus up and down in 1000 element map                                                    │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus up and down in 10000 element map                                                   │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus left and right in a map with 10 rows                                               │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Focus left and right in a map with 100 rows                                              │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Focus left and right in a map with 101 rows                                              │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus left and right in a map with 1000 rows                                             │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Focus left and right in a map with 10000 rows                                            │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Page up and down in 10 element map                                                       │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Page up and down in 100 element map                                                      │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Page up and down in 101 element map                                                      │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Page up and down in 1000 element map                                                     │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Page up and down in 10000 element map                                                    │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -8 (2%)           │ -38 (12%)            │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -8 (2%)           │ -38 (12%)            │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -155 (4%)         │ -1_508 (71%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -155 (4%)         │ -1_508 (71%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Invert ordering of 10 element map                                                        │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Invert ordering of 100 element map                                                       │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    │ Invert ordering of 101 element map                                                       │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Invert ordering of 1000 element map                                                      │ -308 (4%)         │ -3_038 (76%)         │ .                  │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ one cell in it.                                                                          │                   │                      │                    │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ all cells in it.                                                                         │                   │                      │                    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -35 (3%)          │ -308 (48%)           │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -305 (4%)         │ -3_008 (76%)         │ .                  │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Created (foldable - not foldable) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                                          │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                 │ .                    │ .                  │
    │ Focus up and down in 10 element map                                                      │ .                 │ .                    │ .                  │
    │ Focus up and down in 100 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 101 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 1000 element map                                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 10000 element map                                                   │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10 rows                                               │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 100 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 101 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 1000 rows                                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10000 rows                                            │ .                 │ .                    │ .                  │
    │ Page up and down in 10 element map                                                       │ .                 │ .                    │ .                  │
    │ Page up and down in 100 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 101 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 1000 element map                                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 10000 element map                                                    │ .                 │ .                    │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -416 (28%)        │ -992 (98%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -416 (24%)        │ -1_120 (85%)         │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -416 (28%)        │ -992 (98%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -416 (24%)        │ -1_120 (85%)         │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -416 (24%)        │ -1_120 (85%)         │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -936 (27%)        │ -2_232 (93%)         │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -936 (27%)        │ -2_232 (93%)         │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -936 (27%)        │ -2_232 (93%)         │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -5_096 (29%)      │ -12_152 (99%)        │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -5_096 (29%)      │ -12_152 (99%)        │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -10_296 (29%)     │ -24_552 (99%)        │ .                  │
    │ Invert ordering of 10 element map                                                        │ .                 │ .                    │ .                  │
    │ Invert ordering of 100 element map                                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 101 element map                                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 1000 element map                                                      │ .                 │ .                    │ .                  │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ one cell in it.                                                                          │                   │                      │                    │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ all cells in it.                                                                         │                   │                      │                    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                 │ .                    │ .                  │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Recomputed (foldable - not foldable) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                                          │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -5 (3%)           │ -4 (3%)              │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -4 (3%)           │ -4 (3%)              │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -4 (3%)           │ -4 (3%)              │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -4 (3%)           │ -4 (3%)              │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -4 (3%)           │ -4 (3%)              │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -4 (2%)           │ -4 (2%)              │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -4 (1%)           │ -4 (1%)              │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                 │ .                    │ .                  │
    │ Focus up and down in 10 element map                                                      │ .                 │ .                    │ .                  │
    │ Focus up and down in 100 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 101 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 1000 element map                                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 10000 element map                                                   │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10 rows                                               │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 100 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 101 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 1000 rows                                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10000 rows                                            │ .                 │ .                    │ .                  │
    │ Page up and down in 10 element map                                                       │ .                 │ .                    │ .                  │
    │ Page up and down in 100 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 101 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 1000 element map                                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 10000 element map                                                    │ .                 │ .                    │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -52 (2%)          │ -501 (29%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -52 (2%)          │ -501 (28%)           │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -52 (2%)          │ -501 (29%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -52 (2%)          │ -501 (28%)           │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -52 (2%)          │ -501 (28%)           │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -112 (3%)         │ -1_084 (61%)         │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -112 (3%)         │ -1_084 (61%)         │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -112 (3%)         │ -1_084 (61%)         │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -592 (4%)         │ -5_884 (78%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -592 (4%)         │ -5_884 (78%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -1_192 (4%)       │ -11_884 (81%)        │ .                  │
    │ Invert ordering of 10 element map                                                        │ .                 │ +15 (12%)            │ .                  │
    │ Invert ordering of 100 element map                                                       │ .                 │ +105 (21%)           │ .                  │
    │ Invert ordering of 101 element map                                                       │ .                 │ +106 (22%)           │ .                  │
    │ Invert ordering of 1000 element map                                                      │ .                 │ +106 (22%)           │ .                  │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ one cell in it.                                                                          │                   │                      │                    │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ all cells in it.                                                                         │                   │                      │                    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +148 (13%)        │ -337 (31%)           │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +436 (23%)        │ -949 (50%)           │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +148 (13%)        │ -337 (31%)           │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +436 (23%)        │ -949 (50%)           │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +148 (13%)        │ -337 (31%)           │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +436 (23%)        │ -949 (50%)           │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +148 (13%)        │ -337 (31%)           │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +436 (23%)        │ -949 (50%)           │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +1_516 (31%)      │ -3_244 (67%)         │ .                  │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Invalidated (foldable - not foldable) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                                          │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                 │ .                    │ .                  │
    │ Focus up and down in 10 element map                                                      │ .                 │ .                    │ .                  │
    │ Focus up and down in 100 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 101 element map                                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 1000 element map                                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 10000 element map                                                   │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10 rows                                               │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 100 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 101 rows                                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 1000 rows                                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10000 rows                                            │ .                 │ .                    │ .                  │
    │ Page up and down in 10 element map                                                       │ .                 │ .                    │ .                  │
    │ Page up and down in 100 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 101 element map                                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 1000 element map                                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 10000 element map                                                    │ .                 │ .                    │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +704 (129%)       │ -419 (77%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                 │ -106 (45%)           │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +704 (129%)       │ -420 (77%)           │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                 │ -107 (45%)           │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                 │ -17 (28%)            │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +1_584 (177%)     │ -720 (81%)           │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +1_584 (177%)     │ -720 (81%)           │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +1_584 (177%)     │ -720 (81%)           │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +8_624 (211%)     │ -3_920 (96%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +8_624 (211%)     │ -3_920 (96%)         │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +17_424 (215%)    │ -7_920 (98%)         │ .                  │
    │ Invert ordering of 10 element map                                                        │ .                 │ .                    │ .                  │
    │ Invert ordering of 100 element map                                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 101 element map                                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 1000 element map                                                      │ .                 │ .                    │ .                  │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                 │ .                    │ .                  │
    │ cell in it.                                                                              │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ one cell in it.                                                                          │                   │                      │                    │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                 │ .                    │ .                  │
    │ cells in it.                                                                             │                   │                      │                    │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                 │ .                    │ .                  │
    │ all cells in it.                                                                         │                   │                      │                    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                 │ .                    │ .                  │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘
    |}]
;;
