open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "static - dynamic"

(* This test compares dynamic columns to static ones in the new API. We would expect the
   results to be negative, since static should perform better than dynamic.  *)

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

let configs render_cell_kind =
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
  let configs = configs Pure in
  test_startup configs;
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
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ +5 (1%)    │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ +5 (1%)    │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ .                │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ +5 (1%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ .                │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ +5 (1%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ .                │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ .                │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ +5 (1%)    │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ .                │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ .                │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ .                │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ .                │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ +5 (1%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ .                │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ .                │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ .                │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ .                │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ .                │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ .                │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;

let%expect_test "Stateful Rows" =
  let configs = configs Stateful_rows in
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (static - dynamic) =======
    ┌──────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                          │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ +500 (16%) │ +501 (13%)  │ +500 (13%)    │
    │ dynamic cf: 100          │ .          │ .          │ .           │ .             │
    │ dynamic (not cf): 100000 │ .          │ +505 (16%) │ +506 (13%)  │ +505 (13%)    │
    │ dynamic cf: 100000       │ .          │ .          │ .           │ .             │
    └──────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ +50 (9%)         │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +500 (16%)       │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +505 (16%)       │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +505 (16%)       │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +505 (16%)       │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ +50 (9%)         │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map               │ +500 (16%)       │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ +505 (16%)       │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +505 (16%)       │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +505 (16%)       │ .          │
    │ Focus up and down in 10 element map                                     │ +50 (9%)         │ +5 (1%)    │
    │ Focus up and down in 100 element map                                    │ +500 (16%)       │ .          │
    │ Focus up and down in 101 element map                                    │ +505 (16%)       │ .          │
    │ Focus up and down in 1000 element map                                   │ +505 (16%)       │ .          │
    │ Focus up and down in 10000 element map                                  │ +505 (16%)       │ .          │
    │ Focus left and right in a map with 10 rows                              │ +50 (9%)         │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                             │ +500 (16%)       │ .          │
    │ Focus left and right in a map with 101 rows                             │ +505 (16%)       │ .          │
    │ Focus left and right in a map with 1000 rows                            │ +505 (16%)       │ .          │
    │ Focus left and right in a map with 10000 rows                           │ +505 (16%)       │ .          │
    │ Page up and down in 10 element map                                      │ +50 (9%)         │ +5 (1%)    │
    │ Page up and down in 100 element map                                     │ +500 (16%)       │ .          │
    │ Page up and down in 101 element map                                     │ +505 (16%)       │ .          │
    │ Page up and down in 1000 element map                                    │ +505 (16%)       │ .          │
    │ Page up and down in 10000 element map                                   │ +505 (16%)       │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +5 (2%)          │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +50 (9%)         │ +5 (1%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +5 (2%)          │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +50 (9%)         │ +5 (1%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +500 (16%)       │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +50 (9%)         │ +5 (1%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +50 (9%)         │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +50 (9%)         │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +250 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +250 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +500 (16%)       │ .          │
    │ Invert ordering of 10 element map                                       │ +50 (9%)         │ +5 (1%)    │
    │ Invert ordering of 100 element map                                      │ +500 (16%)       │ .          │
    │ Invert ordering of 101 element map                                      │ +505 (16%)       │ .          │
    │ Invert ordering of 1000 element map                                     │ +505 (16%)       │ .          │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9%)         │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9%)         │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9%)         │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9%)         │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9%)         │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +50 (9%)         │ +5 (1%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +500 (16%)       │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (31%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +80 (14%)        │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (31%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +80 (14%)        │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +80 (14%)        │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (25%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (25%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (25%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (31%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (31%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (32%)     │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -1 (2%)          │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (5%)         │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +80 (4%)         │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (5%)         │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +80 (4%)         │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +80 (4%)         │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (13%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (17%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (17%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (18%)     │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +275 (18%)       │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +95 (10%)        │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +275 (18%)       │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +950 (25%)       │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (18%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (18%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (26%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (26%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (26%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (33%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (33%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (35%)     │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;

let%expect_test "Stateful_cells" =
  let configs = configs Stateful_cells in
  test_startup configs;
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
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -130 (12%)       │ +5 (1%)    │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -1_300 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -1_313 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -1_313 (16%)     │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ -130 (12%)       │ +5 (1%)    │
    │ Focus by key (key present) and unfocus in 100 element map               │ -1_300 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -1_313 (16%)     │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -1_313 (16%)     │ .          │
    │ Focus up and down in 10 element map                                     │ -130 (12%)       │ +5 (1%)    │
    │ Focus up and down in 100 element map                                    │ -1_300 (16%)     │ .          │
    │ Focus up and down in 101 element map                                    │ -1_313 (16%)     │ .          │
    │ Focus up and down in 1000 element map                                   │ -1_313 (16%)     │ .          │
    │ Focus up and down in 10000 element map                                  │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 10 rows                              │ -130 (12%)       │ +5 (1%)    │
    │ Focus left and right in a map with 100 rows                             │ -1_300 (16%)     │ .          │
    │ Focus left and right in a map with 101 rows                             │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 1000 rows                            │ -1_313 (16%)     │ .          │
    │ Focus left and right in a map with 10000 rows                           │ -1_313 (16%)     │ .          │
    │ Page up and down in 10 element map                                      │ -130 (12%)       │ +5 (1%)    │
    │ Page up and down in 100 element map                                     │ -1_300 (16%)     │ .          │
    │ Page up and down in 101 element map                                     │ -1_313 (16%)     │ .          │
    │ Page up and down in 1000 element map                                    │ -1_313 (16%)     │ .          │
    │ Page up and down in 10000 element map                                   │ -1_313 (16%)     │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -13 (4%)         │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -130 (12%)       │ +5 (1%)    │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -13 (4%)         │ +5 (2%)    │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -130 (12%)       │ +5 (1%)    │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_300 (16%)     │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -130 (12%)       │ +5 (1%)    │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -130 (12%)       │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -130 (12%)       │ +5 (1%)    │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -650 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -650 (15%)       │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -1_300 (16%)     │ .          │
    │ Invert ordering of 10 element map                                       │ -130 (12%)       │ +5 (1%)    │
    │ Invert ordering of 100 element map                                      │ -1_300 (16%)     │ .          │
    │ Invert ordering of 101 element map                                      │ -1_313 (16%)     │ .          │
    │ Invert ordering of 1000 element map                                     │ -1_313 (16%)     │ .          │
    │ Randomly select a row, then change one cell in it.                      │ -130 (12%)       │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ -130 (12%)       │ +5 (1%)    │
    │ Randomly select a row, then change one cell in it.                      │ -130 (12%)       │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ -130 (12%)       │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ -130 (12%)       │ +5 (1%)    │
    │ Randomly select a row, then change all cells in it.                     │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -130 (12%)       │ +5 (1%)    │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -1_300 (16%)     │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -576 (39%)       │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -576 (32%)       │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -576 (39%)       │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -576 (32%)       │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -576 (32%)       │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_296 (37%)     │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_296 (37%)     │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_296 (37%)     │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -7_056 (39%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -7_056 (39%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -14_256 (39%)    │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -1 (2%)          │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -208 (8%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -208 (8%)        │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -208 (8%)        │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -208 (8%)        │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -208 (8%)        │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -468 (14%)       │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_548 (16%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_548 (16%)     │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -5_148 (16%)     │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +220 (12%)       │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +76 (7%)         │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +220 (12%)       │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +760 (15%)       │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .          │
    │ Focus up and down in 10 element map                                     │ .                │ .          │
    │ Focus up and down in 100 element map                                    │ .                │ .          │
    │ Focus up and down in 101 element map                                    │ .                │ .          │
    │ Focus up and down in 1000 element map                                   │ .                │ .          │
    │ Focus up and down in 10000 element map                                  │ .                │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .          │
    │ Page up and down in 10 element map                                      │ .                │ .          │
    │ Page up and down in 100 element map                                     │ .                │ .          │
    │ Page up and down in 101 element map                                     │ .                │ .          │
    │ Page up and down in 1000 element map                                    │ .                │ .          │
    │ Page up and down in 10000 element map                                   │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +544 (102%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +544 (102%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +1_224 (136%)    │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +1_224 (136%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +1_224 (136%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +6_664 (162%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +6_664 (162%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +13_464 (166%)   │ .          │
    │ Invert ordering of 10 element map                                       │ .                │ .          │
    │ Invert ordering of 100 element map                                      │ .                │ .          │
    │ Invert ordering of 101 element map                                      │ .                │ .          │
    │ Invert ordering of 1000 element map                                     │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘
    |}]
;;
