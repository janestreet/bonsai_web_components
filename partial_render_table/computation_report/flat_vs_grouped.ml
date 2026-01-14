open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "Flat - Grouped"

(* This test compares flat vs grouped column structures. We expect the grouped tests to be
   a little slower / have a few more nodes than the flat ones, but not by a ton. So these
   numbers will probably be positive, but shouldn't be too huge. *)

let test_startup pairs =
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    (Symbol_table.startup_inputs [ 100; 100_000 ])
;;

let%expect_test "Flat -> Grouped" =
  let pairs =
    [ ( "dyn cells, counters"
      , Config.Dynamic_cells
          { counters_in_cells = true; col_groups = true; duplicate_col = false }
      , Config.Dynamic_cells
          { counters_in_cells = true; col_groups = false; duplicate_col = false } )
    ; ( "dyn cells, no counters"
      , Config.Dynamic_cells
          { counters_in_cells = false; col_groups = true; duplicate_col = false }
      , Config.Dynamic_cells
          { counters_in_cells = false; col_groups = false; duplicate_col = false } )
    ; ( "dyn cols, counters"
      , Dynamic_cols
          { which_dynamic_cols = Counters; col_groups = true; duplicate_col = false }
      , Dynamic_cols
          { which_dynamic_cols = Counters; col_groups = false; duplicate_col = false } )
    ; ( "dyn cols, no counters, cf"
      , Dynamic_cols
          { which_dynamic_cols = No_counters_constant_foldable
          ; col_groups = true
          ; duplicate_col = false
          }
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
    ======= Startup Incr Node Stats (Flat - Grouped) =======
    ┌───────────────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                                   │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├───────────────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ dyn cells, counters: 100          │ .          │ +1_688 (15%) │ +1_978 (15%) │ +1_981 (15%)  │
    │ dyn cells, no counters: 100       │ .          │ -15 (1%)     │ -27 (2%)     │ -24 (2%)      │
    │ dyn cols, counters: 100           │ .          │ .            │ .            │ .             │
    │ dyn cols, no counters, cf: 100    │ .          │ .            │ .            │ .             │
    │ dyn cells, counters: 100000       │ .          │ +1_705 (15%) │ +1_998 (15%) │ +2_001 (15%)  │
    │ dyn cells, no counters: 100000    │ .          │ -15 (1%)     │ -27 (2%)     │ -24 (2%)      │
    │ dyn cols, counters: 100000        │ .          │ .            │ .            │ .             │
    │ dyn cols, no counters, cf: 100000 │ .          │ .            │ .            │ .             │
    └───────────────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (Flat - Grouped) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                                          │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map                                │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map                                │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus up and down in 10 element map                                                      │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Focus up and down in 100 element map                                                     │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus up and down in 101 element map                                                     │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                                    │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                                   │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                                               │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                                              │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                                              │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                                             │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                                            │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Page up and down in 10 element map                                                       │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Page up and down in 100 element map                                                      │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Page up and down in 101 element map                                                      │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Page up and down in 1000 element map                                                     │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Page up and down in 10000 element map                                                    │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +5 (1%)             │ -15 (5%)               │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +5 (1%)             │ -15 (5%)               │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +838 (14%)          │ -15 (2%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +838 (14%)          │ -15 (2%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Invert ordering of 10 element map                                                        │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Invert ordering of 100 element map                                                       │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Invert ordering of 101 element map                                                       │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                                      │ +1_705 (15%)        │ -15 (1%)               │ .                  │ .                         │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ one cell in it.                                                                          │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ all cells in it.                                                                         │                     │                        │                    │                           │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +158 (11%)          │ -15 (4%)               │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +1_688 (15%)        │ -15 (1%)               │ .                  │ .                         │
    └──────────────────────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Created (Flat - Grouped) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                                          │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 100 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 101 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                                   │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                                               │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                                            │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 100 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 101 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 1000 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +192 (16%)          │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +320 (15%)          │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +192 (16%)          │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +320 (15%)          │ .                      │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +320 (15%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +2_352 (16%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +2_352 (16%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +4_752 (17%)        │ .                      │ .                  │ .                         │
    │ Invert ordering of 10 element map                                                        │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 100 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 101 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ one cell in it.                                                                          │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ all cells in it.                                                                         │                     │                        │                    │                           │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                   │ .                      │ .                  │ .                         │
    └──────────────────────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Recomputed (Flat - Grouped) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                                          │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -7 (5%)             │ -3 (2%)                │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -6 (4%)             │ -3 (2%)                │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -6 (4%)             │ -3 (2%)                │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -6 (4%)             │ -3 (2%)                │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -6 (4%)             │ -3 (2%)                │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -6 (3%)             │ -3 (1%)                │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -6 (1%)             │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map                                │ -6 (1%)             │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ -6 (1%)             │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ -6 (1%)             │ .                      │ .                  │ .                         │
    │ Focus up and down in 10 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 100 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 101 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                                   │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                                               │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                                            │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 100 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 101 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 1000 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +266 (7%)           │ -20 (1%)               │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +266 (7%)           │ -20 (1%)               │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +266 (7%)           │ -20 (1%)               │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +266 (7%)           │ -20 (1%)               │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +266 (7%)           │ -20 (1%)               │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +602 (13%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +602 (13%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +602 (13%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +3_322 (15%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +3_322 (15%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +6_722 (15%)        │ .                      │ .                  │ .                         │
    │ Invert ordering of 10 element map                                                        │ -28 (7%)            │ .                      │ .                  │ .                         │
    │ Invert ordering of 100 element map                                                       │ -118 (4%)           │ .                      │ .                  │ .                         │
    │ Invert ordering of 101 element map                                                       │ -119 (4%)           │ .                      │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                                      │ -119 (4%)           │ .                      │ .                  │ .                         │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ one cell in it.                                                                          │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ all cells in it.                                                                         │                     │                        │                    │                           │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +165 (7%)           │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +489 (11%)          │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +165 (7%)           │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +489 (11%)          │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +165 (7%)           │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +489 (11%)          │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +165 (7%)           │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +489 (11%)          │ -13 (1%)               │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +1_704 (14%)        │ .                      │ .                  │ .                         │
    └──────────────────────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Invalidated (Flat - Grouped) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                                          │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 100 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 101 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                                   │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                                               │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                                            │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 100 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 101 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 1000 element map                                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10000 element map                                                    │ .                   │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +291 (16%)          │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +106 (14%)          │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +292 (16%)          │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +107 (14%)          │ .                      │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +17 (12%)           │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +432 (16%)          │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +2_352 (16%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +2_352 (16%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +4_752 (17%)        │ .                      │ .                  │ .                         │
    │ Invert ordering of 10 element map                                                        │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 100 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 101 element map                                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                   │ .                      │ .                  │ .                         │
    │ cell in it.                                                                              │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ one cell in it.                                                                          │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                   │ .                      │ .                  │ .                         │
    │ cells in it.                                                                             │                     │                        │                    │                           │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                   │ .                      │ .                  │ .                         │
    │ all cells in it.                                                                         │                     │                        │                    │                           │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                   │ .                      │ .                  │ .                         │
    └──────────────────────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘
    |}]
;;
