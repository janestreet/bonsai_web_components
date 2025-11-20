open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "static - dynamic"

(* This test compares dynamic columns to static ones in the new API. We would expect the
   results to be negative, since static should perform better than dynamic. *)

let test_startup pairs =
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    (Symbol_table.startup_inputs [ 100; 100_000 ])
;;

let pairs render_cell_kind =
  [ ( "dynamic (not cf)"
    , Config.New_api
        { counters_in_cells = true
        ; cols = Dynamic
        ; col_groups = false
        ; render_cell_kind
        ; duplicate_col = false
        }
    , Config.New_api
        { counters_in_cells = true
        ; cols = Static
        ; col_groups = true
        ; render_cell_kind
        ; duplicate_col = false
        } )
  ; ( "dynamic cf"
    , Config.New_api
        { counters_in_cells = false
        ; cols = Dynamic_constant_foldable
        ; col_groups = false
        ; render_cell_kind
        ; duplicate_col = false
        }
    , Config.New_api
        { counters_in_cells = false
        ; cols = Static
        ; col_groups = true
        ; render_cell_kind
        ; duplicate_col = false
        } )
  ]
;;

let%expect_test "Pure" =
  let pairs = pairs Pure in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (static - dynamic) =======
    ┌──────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                          │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ .          │ .           │ .             │
    │ dynamic cf: 100          │ .          │ .          │ .           │ .             │
    │ dynamic (not cf): 100000 │ .          │ .          │ .           │ .             │
    │ dynamic cf: 100000       │ .          │ .          │ .           │ .             │
    └──────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ +5 (1%)    │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ +5 (1%)    │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ .                │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ +5 (1%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ .                │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ +5 (1%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ .                │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ +5 (1%)    │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ +5 (1%)    │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ +5 (1%)    │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ +5 (1%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ .                │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ +3 (2%)          │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ +4 (3%)          │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ +4 (3%)          │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ +4 (3%)          │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ +4 (3%)          │ +2 (1%)    │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ +4 (2%)          │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ +4 (1%)          │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ +4 (1%)          │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ +4 (1%)          │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ +4 (1%)          │ .          │
    │ Focus up and down in 10 element map                                                      │ +2 (3%)          │ .          │
    │ Focus up and down in 100 element map                                                     │ +2 (1%)          │ .          │
    │ Focus up and down in 101 element map                                                     │ +2 (1%)          │ .          │
    │ Focus up and down in 1000 element map                                                    │ +2 (1%)          │ .          │
    │ Focus up and down in 10000 element map                                                   │ +2 (1%)          │ .          │
    │ Focus left and right in a map with 10 rows                                               │ +2 (3%)          │ .          │
    │ Focus left and right in a map with 100 rows                                              │ +2 (1%)          │ .          │
    │ Focus left and right in a map with 101 rows                                              │ +2 (1%)          │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ +2 (1%)          │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ +2 (1%)          │ .          │
    │ Page up and down in 10 element map                                                       │ +2 (3%)          │ .          │
    │ Page up and down in 100 element map                                                      │ +2 (1%)          │ .          │
    │ Page up and down in 101 element map                                                      │ +2 (1%)          │ .          │
    │ Page up and down in 1000 element map                                                     │ +2 (1%)          │ .          │
    │ Page up and down in 10000 element map                                                    │ +2 (1%)          │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ .                │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ .                │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;

let%expect_test "Stateful Rows" =
  let pairs = pairs Stateful_rows in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (static - dynamic) =======
    ┌──────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                          │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ +500 (17%) │ +501 (14%)  │ +500 (14%)    │
    │ dynamic cf: 100          │ .          │ .          │ .           │ .             │
    │ dynamic (not cf): 100000 │ .          │ +505 (17%) │ +506 (14%)  │ +505 (14%)    │
    │ dynamic cf: 100000       │ .          │ .          │ .           │ .             │
    └──────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ +50 (9%)         │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ +500 (17%)       │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ +505 (17%)       │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ +505 (17%)       │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ +505 (17%)       │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ +50 (9%)         │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map                                │ +500 (17%)       │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ +505 (17%)       │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ +505 (17%)       │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ +505 (17%)       │ .          │
    │ Focus up and down in 10 element map                                                      │ +50 (9%)         │ +5 (1%)    │
    │ Focus up and down in 100 element map                                                     │ +500 (17%)       │ .          │
    │ Focus up and down in 101 element map                                                     │ +505 (17%)       │ .          │
    │ Focus up and down in 1000 element map                                                    │ +505 (17%)       │ .          │
    │ Focus up and down in 10000 element map                                                   │ +505 (17%)       │ .          │
    │ Focus left and right in a map with 10 rows                                               │ +50 (9%)         │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                                              │ +500 (17%)       │ .          │
    │ Focus left and right in a map with 101 rows                                              │ +505 (17%)       │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ +505 (17%)       │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ +505 (17%)       │ .          │
    │ Page up and down in 10 element map                                                       │ +50 (9%)         │ +5 (1%)    │
    │ Page up and down in 100 element map                                                      │ +500 (17%)       │ .          │
    │ Page up and down in 101 element map                                                      │ +505 (17%)       │ .          │
    │ Page up and down in 1000 element map                                                     │ +505 (17%)       │ .          │
    │ Page up and down in 10000 element map                                                    │ +505 (17%)       │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +5 (2%)          │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +50 (9%)         │ +5 (2%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +5 (2%)          │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +50 (9%)         │ +5 (2%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +500 (17%)       │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +50 (9%)         │ +5 (2%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +50 (9%)         │ +5 (2%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +50 (9%)         │ +5 (2%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +250 (16%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +250 (16%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +500 (17%)       │ .          │
    │ Invert ordering of 10 element map                                                        │ +50 (9%)         │ +5 (1%)    │
    │ Invert ordering of 100 element map                                                       │ +500 (17%)       │ .          │
    │ Invert ordering of 101 element map                                                       │ +505 (17%)       │ .          │
    │ Invert ordering of 1000 element map                                                      │ +505 (17%)       │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ +50 (9%)         │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ +50 (9%)         │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ +50 (9%)         │ +5 (1%)    │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ +50 (9%)         │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ +50 (9%)         │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ +50 (9%)         │ +5 (1%)    │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +50 (9%)         │ +5 (2%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +500 (17%)       │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +80 (32%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +80 (15%)        │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +80 (32%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +80 (15%)        │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +80 (15%)        │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +980 (34%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +980 (34%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +1_980 (35%)     │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ +2 (1%)    │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +80 (5%)         │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ +80 (5%)         │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +80 (5%)         │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ +80 (5%)         │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ +80 (5%)         │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +980 (18%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +980 (18%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +1_980 (19%)     │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +275 (18%)       │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +950 (26%)       │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +80 (18%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +80 (18%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +180 (27%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +980 (34%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +980 (34%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +1_980 (35%)     │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;

let%expect_test "Stateful_cells" =
  let pairs = pairs Stateful_cells in
  test_startup pairs;
  [%expect
    {|
    ======= Startup Incr Node Stats (static - dynamic) =======
    ┌──────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                          │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├──────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ -1_300 (16%) │ -3_599 (32%) │ -3_600 (32%)  │
    │ dynamic cf: 100          │ .          │ .            │ .            │ .             │
    │ dynamic (not cf): 100000 │ .          │ -1_313 (16%) │ -3_635 (32%) │ -3_636 (32%)  │
    │ dynamic cf: 100000       │ .          │ .            │ .            │ .             │
    └──────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs
    ~title
    ~get_inject:Config.get_inject
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -130 (12%)       │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -1_300 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -1_313 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -1_313 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -130 (12%)       │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -1_300 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ -1_313 (16%)     │ .          │
    │ Focus up and down in 10 element map                                                      │ -130 (12%)       │ +5 (1%)    │
    │ Focus up and down in 100 element map                                                     │ -1_300 (16%)     │ .          │
    │ Focus up and down in 101 element map                                                     │ -1_313 (16%)     │ .          │
    │ Focus up and down in 1000 element map                                                    │ -1_313 (16%)     │ .          │
    │ Focus up and down in 10000 element map                                                   │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 10 rows                                               │ -130 (12%)       │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                                              │ -1_300 (16%)     │ .          │
    │ Focus left and right in a map with 101 rows                                              │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ -1_313 (16%)     │ .          │
    │ Page up and down in 10 element map                                                       │ -130 (12%)       │ +5 (1%)    │
    │ Page up and down in 100 element map                                                      │ -1_300 (16%)     │ .          │
    │ Page up and down in 101 element map                                                      │ -1_313 (16%)     │ .          │
    │ Page up and down in 1000 element map                                                     │ -1_313 (16%)     │ .          │
    │ Page up and down in 10000 element map                                                    │ -1_313 (16%)     │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -13 (4%)         │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -130 (12%)       │ +5 (2%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -13 (4%)         │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -130 (12%)       │ +5 (2%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -1_300 (16%)     │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -130 (12%)       │ +5 (2%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -130 (12%)       │ +5 (2%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -130 (12%)       │ +5 (2%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -650 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -650 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -1_300 (16%)     │ .          │
    │ Invert ordering of 10 element map                                                        │ -130 (12%)       │ +5 (1%)    │
    │ Invert ordering of 100 element map                                                       │ -1_300 (16%)     │ .          │
    │ Invert ordering of 101 element map                                                       │ -1_313 (16%)     │ .          │
    │ Invert ordering of 1000 element map                                                      │ -1_313 (16%)     │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ -130 (12%)       │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ -130 (12%)       │ +5 (1%)    │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -130 (12%)       │ +5 (1%)    │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ -130 (12%)       │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ -130 (12%)       │ +5 (1%)    │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -130 (12%)       │ +5 (1%)    │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -130 (12%)       │ +5 (2%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -1_300 (16%)     │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -576 (39%)       │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -576 (33%)       │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -576 (39%)       │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -576 (33%)       │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -576 (33%)       │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -1_296 (38%)     │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -1_296 (38%)     │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -1_296 (38%)     │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -7_056 (40%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -7_056 (40%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -14_256 (40%)    │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ +2 (1%)    │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ +2 (1%)    │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -208 (9%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -208 (9%)        │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -208 (9%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -208 (9%)        │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -208 (9%)        │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -2_548 (16%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -2_548 (16%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -5_148 (16%)     │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ +220 (12%)       │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ +760 (16%)       │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                                          │ dynamic (not cf) │ dynamic cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .                │ .          │
    │ Focus up and down in 10 element map                                                      │ .                │ .          │
    │ Focus up and down in 100 element map                                                     │ .                │ .          │
    │ Focus up and down in 101 element map                                                     │ .                │ .          │
    │ Focus up and down in 1000 element map                                                    │ .                │ .          │
    │ Focus up and down in 10000 element map                                                   │ .                │ .          │
    │ Focus left and right in a map with 10 rows                                               │ .                │ .          │
    │ Focus left and right in a map with 100 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 101 rows                                              │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                                             │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                                            │ .                │ .          │
    │ Page up and down in 10 element map                                                       │ .                │ .          │
    │ Page up and down in 100 element map                                                      │ .                │ .          │
    │ Page up and down in 101 element map                                                      │ .                │ .          │
    │ Page up and down in 1000 element map                                                     │ .                │ .          │
    │ Page up and down in 10000 element map                                                    │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ +544 (100%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ +544 (100%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ +1_224 (137%)    │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ +1_224 (137%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ +1_224 (137%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ +6_664 (163%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ +6_664 (163%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ +13_464 (166%)   │ .          │
    │ Invert ordering of 10 element map                                                        │ .                │ .          │
    │ Invert ordering of 100 element map                                                       │ .                │ .          │
    │ Invert ordering of 101 element map                                                       │ .                │ .          │
    │ Invert ordering of 1000 element map                                                      │ .                │ .          │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .                │ .          │
    │ cell in it.                                                                              │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ one cell in it.                                                                          │                  │            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .                │ .          │
    │ cells in it.                                                                             │                  │            │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .                │ .          │
    │ all cells in it.                                                                         │                  │            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .                │ .          │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;
