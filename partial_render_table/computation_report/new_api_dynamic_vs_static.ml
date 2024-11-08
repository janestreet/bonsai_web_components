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
    ┌──────────────────────────┬────────────┬─────────────┬─────────────┬───────────────┐
    │                          │ max_height │ node_count  │ max_node_id │ nodes_created │
    ├──────────────────────────┼────────────┼─────────────┼─────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ -1 (-0.09%) │ .           │ -1 (-0.07%)   │
    │ dynamic cf: 100          │ .          │ +5 (0.47%)  │ +7 (0.47%)  │ +5 (0.33%)    │
    │ dynamic (not cf): 100000 │ .          │ -1 (-0.09%) │ .           │ -1 (-0.07%)   │
    │ dynamic cf: 100000       │ .          │ +5 (0.47%)  │ +7 (0.46%)  │ +5 (0.33%)    │
    └──────────────────────────┴────────────┴─────────────┴─────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10 element map                │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Focus by key (key present) and unfocus in 100 element map               │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 101 element map               │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus up and down in 10 element map                                     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Focus up and down in 100 element map                                    │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus up and down in 101 element map                                    │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus up and down in 1000 element map                                   │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus up and down in 10000 element map                                  │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus left and right in a map with 10 rows                              │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Focus left and right in a map with 100 rows                             │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus left and right in a map with 101 rows                             │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus left and right in a map with 1000 rows                            │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Focus left and right in a map with 10000 rows                           │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Page up and down in 10 element map                                      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Page up and down in 100 element map                                     │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Page up and down in 101 element map                                     │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Page up and down in 1000 element map                                    │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Page up and down in 10000 element map                                   │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1 (-0.35%)      │ +5 (1.81%) │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1 (-0.28%)      │ +5 (1.44%) │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1 (-0.35%)      │ +5 (1.81%) │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1 (-0.28%)      │ +5 (1.44%) │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -1 (-0.15%)      │ +5 (0.75%) │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -1 (-0.15%)      │ +5 (0.75%) │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Invert ordering of 10 element map                                       │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Invert ordering of 100 element map                                      │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Invert ordering of 101 element map                                      │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Invert ordering of 1000 element map                                     │ -1 (-0.09%)      │ +5 (0.47%) │
    │ Randomly select a row, then change one cell in it.                      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Randomly select a row, then change one cell in it.                      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Randomly select a row, then change one cell in it.                      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Randomly select a row, then change all cells in it.                     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Randomly select a row, then change all cells in it.                     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Randomly select a row, then change all cells in it.                     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -1 (-0.28%)      │ +5 (1.45%) │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -1 (-0.09%)      │ +5 (0.47%) │
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
    ┌──────────────────────────┬────────────┬───────────────┬───────────────┬───────────────┐
    │                          │ max_height │ node_count    │ max_node_id   │ nodes_created │
    ├──────────────────────────┼────────────┼───────────────┼───────────────┼───────────────┤
    │ dynamic (not cf): 100    │ .          │ +500 (16.30%) │ +501 (13.16%) │ +500 (13.14%) │
    │ dynamic cf: 100          │ .          │ +5 (0.47%)    │ +7 (0.47%)    │ +5 (0.33%)    │
    │ dynamic (not cf): 100000 │ .          │ +505 (16.31%) │ +506 (13.17%) │ +505 (13.15%) │
    │ dynamic cf: 100000       │ .          │ +5 (0.47%)    │ +7 (0.46%)    │ +5 (0.33%)    │
    └──────────────────────────┴────────────┴───────────────┴───────────────┴───────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ +50 (9.12%)      │ +5 (1.47%) │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +500 (16.30%)    │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10 element map                │ +50 (9.12%)      │ +5 (1.47%) │
    │ Focus by key (key present) and unfocus in 100 element map               │ +500 (16.30%)    │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 101 element map               │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus up and down in 10 element map                                     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Focus up and down in 100 element map                                    │ +500 (16.30%)    │ +5 (0.47%) │
    │ Focus up and down in 101 element map                                    │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus up and down in 1000 element map                                   │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus up and down in 10000 element map                                  │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus left and right in a map with 10 rows                              │ +50 (9.12%)      │ +5 (1.47%) │
    │ Focus left and right in a map with 100 rows                             │ +500 (16.30%)    │ +5 (0.47%) │
    │ Focus left and right in a map with 101 rows                             │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus left and right in a map with 1000 rows                            │ +505 (16.31%)    │ +5 (0.47%) │
    │ Focus left and right in a map with 10000 rows                           │ +505 (16.31%)    │ +5 (0.47%) │
    │ Page up and down in 10 element map                                      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Page up and down in 100 element map                                     │ +500 (16.30%)    │ +5 (0.47%) │
    │ Page up and down in 101 element map                                     │ +505 (16.31%)    │ +5 (0.47%) │
    │ Page up and down in 1000 element map                                    │ +505 (16.31%)    │ +5 (0.47%) │
    │ Page up and down in 10000 element map                                   │ +505 (16.31%)    │ +5 (0.47%) │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +5 (1.67%)       │ +5 (1.85%) │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +50 (9.06%)      │ +5 (1.46%) │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +5 (1.67%)       │ +5 (1.85%) │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +50 (9.06%)      │ +5 (1.46%) │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +500 (16.28%)    │ +5 (0.47%) │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +50 (9.11%)      │ +5 (1.47%) │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +50 (9.11%)      │ +5 (1.47%) │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +50 (9.11%)      │ +5 (1.47%) │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +250 (14.98%)    │ +5 (0.76%) │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +250 (14.98%)    │ +5 (0.76%) │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +500 (16.29%)    │ +5 (0.47%) │
    │ Invert ordering of 10 element map                                       │ +50 (9.11%)      │ +5 (1.47%) │
    │ Invert ordering of 100 element map                                      │ +500 (16.29%)    │ +5 (0.47%) │
    │ Invert ordering of 101 element map                                      │ +505 (16.31%)    │ +5 (0.47%) │
    │ Invert ordering of 1000 element map                                     │ +505 (16.31%)    │ +5 (0.47%) │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Randomly select a row, then change one cell in it.                      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +50 (9.12%)      │ +5 (1.47%) │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +500 (16.30%)    │ +5 (0.47%) │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (31.13%)     │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +80 (13.86%)     │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (31.13%)     │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +80 (13.86%)     │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +80 (13.86%)     │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (24.73%)    │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (24.73%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (24.73%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (31.33%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (31.33%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (32.31%)  │ .          │
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
    │ Focus by key (key not present) and unfocus in 10 element map            │ -1 (-1.75%)      │ .          │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (4.50%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +80 (4.46%)      │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (4.50%)      │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +80 (4.46%)      │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +80 (4.46%)      │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (13.04%)    │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (13.04%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (13.04%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (17.19%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (17.19%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (17.84%)  │ .          │
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
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +95 (10.29%)     │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +275 (17.92%)    │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +95 (10.29%)     │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +275 (17.92%)    │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +95 (10.29%)     │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +275 (17.92%)    │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +95 (10.29%)     │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +275 (17.92%)    │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +950 (24.80%)    │ .          │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +80 (18.39%)     │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +80 (18.31%)     │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +180 (26.20%)    │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +180 (26.20%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +180 (26.20%)    │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +980 (33.48%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +980 (33.48%)    │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_980 (34.57%)  │ .          │
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
    ┌──────────────────────────┬────────────┬──────────────────┬──────────────────┬──────────────────┐
    │                          │ max_height │ node_count       │ max_node_id      │ nodes_created    │
    ├──────────────────────────┼────────────┼──────────────────┼──────────────────┼──────────────────┤
    │ dynamic (not cf): 100    │ .          │ -1_300 (-15.54%) │ -3_599 (-31.55%) │ -3_600 (-31.56%) │
    │ dynamic cf: 100          │ .          │ +5 (0.47%)       │ +7 (0.47%)       │ +5 (0.33%)       │
    │ dynamic (not cf): 100000 │ .          │ -1_313 (-15.54%) │ -3_635 (-31.56%) │ -3_636 (-31.57%) │
    │ dynamic cf: 100000       │ .          │ +5 (0.47%)       │ +7 (0.46%)       │ +5 (0.33%)       │
    └──────────────────────────┴────────────┴──────────────────┴──────────────────┴──────────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -1_300 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10 element map                │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Focus by key (key present) and unfocus in 100 element map               │ -1_300 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 101 element map               │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus up and down in 10 element map                                     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Focus up and down in 100 element map                                    │ -1_300 (-15.54%) │ +5 (0.47%) │
    │ Focus up and down in 101 element map                                    │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus up and down in 1000 element map                                   │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus up and down in 10000 element map                                  │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus left and right in a map with 10 rows                              │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Focus left and right in a map with 100 rows                             │ -1_300 (-15.54%) │ +5 (0.47%) │
    │ Focus left and right in a map with 101 rows                             │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus left and right in a map with 1000 rows                            │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Focus left and right in a map with 10000 rows                           │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Page up and down in 10 element map                                      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Page up and down in 100 element map                                     │ -1_300 (-15.54%) │ +5 (0.47%) │
    │ Page up and down in 101 element map                                     │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Page up and down in 1000 element map                                    │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Page up and down in 10000 element map                                   │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -13 (-3.68%)     │ +5 (1.85%) │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -130 (-12.01%)   │ +5 (1.46%) │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -13 (-3.68%)     │ +5 (1.85%) │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -130 (-12.01%)   │ +5 (1.46%) │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_300 (-15.53%) │ +5 (0.47%) │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -130 (-12.05%)   │ +5 (1.47%) │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -130 (-12.05%)   │ +5 (1.47%) │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -130 (-12.05%)   │ +5 (1.47%) │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -650 (-15.05%)   │ +5 (0.76%) │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -650 (-15.05%)   │ +5 (0.76%) │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -1_300 (-15.53%) │ +5 (0.47%) │
    │ Invert ordering of 10 element map                                       │ -130 (-12.05%)   │ +5 (1.47%) │
    │ Invert ordering of 100 element map                                      │ -1_300 (-15.53%) │ +5 (0.47%) │
    │ Invert ordering of 101 element map                                      │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Invert ordering of 1000 element map                                     │ -1_313 (-15.54%) │ +5 (0.47%) │
    │ Randomly select a row, then change one cell in it.                      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Randomly select a row, then change one cell in it.                      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Randomly select a row, then change one cell in it.                      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Randomly select a row, then change all cells in it.                     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -130 (-12.06%)   │ +5 (1.47%) │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -1_300 (-15.54%) │ +5 (0.47%) │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Created (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬────────────┐
    │                                                                         │ dynamic (not cf)  │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .          │
    │ Focus up and down in 10 element map                                     │ .                 │ .          │
    │ Focus up and down in 100 element map                                    │ .                 │ .          │
    │ Focus up and down in 101 element map                                    │ .                 │ .          │
    │ Focus up and down in 1000 element map                                   │ .                 │ .          │
    │ Focus up and down in 10000 element map                                  │ .                 │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .          │
    │ Page up and down in 10 element map                                      │ .                 │ .          │
    │ Page up and down in 100 element map                                     │ .                 │ .          │
    │ Page up and down in 101 element map                                     │ .                 │ .          │
    │ Page up and down in 1000 element map                                    │ .                 │ .          │
    │ Page up and down in 10000 element map                                   │ .                 │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -576 (-39.10%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -576 (-32.12%)    │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -576 (-39.10%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -576 (-32.12%)    │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -576 (-32.12%)    │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_296 (-37.41%)  │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_296 (-37.41%)  │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_296 (-37.41%)  │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -7_056 (-39.15%)  │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -7_056 (-39.15%)  │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -14_256 (-39.36%) │ .          │
    │ Invert ordering of 10 element map                                       │ .                 │ .          │
    │ Invert ordering of 100 element map                                      │ .                 │ .          │
    │ Invert ordering of 101 element map                                      │ .                 │ .          │
    │ Invert ordering of 1000 element map                                     │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                 │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                 │ .          │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴────────────┘

    ====== Nodes Recomputed (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬────────────┐
    │                                                                         │ dynamic (not cf) │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -1 (-1.75%)      │ .          │
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
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -208 (-7.92%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -208 (-7.88%)    │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -208 (-7.92%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -208 (-7.88%)    │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -208 (-7.88%)    │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -468 (-14.23%)   │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -468 (-14.23%)   │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -468 (-14.23%)   │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_548 (-15.84%) │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_548 (-15.84%) │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -5_148 (-16.04%) │ .          │
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
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +76 (7.33%)      │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +220 (11.80%)    │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +76 (7.33%)      │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +220 (11.80%)    │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +76 (7.33%)      │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +220 (11.80%)    │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +76 (7.33%)      │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +220 (11.80%)    │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +760 (15.29%)    │ .          │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴────────────┘

    ====== Nodes Invalidated (static - dynamic) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬────────────┐
    │                                                                         │ dynamic (not cf)  │ dynamic cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .          │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .          │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .          │
    │ Focus up and down in 10 element map                                     │ .                 │ .          │
    │ Focus up and down in 100 element map                                    │ .                 │ .          │
    │ Focus up and down in 101 element map                                    │ .                 │ .          │
    │ Focus up and down in 1000 element map                                   │ .                 │ .          │
    │ Focus up and down in 10000 element map                                  │ .                 │ .          │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .          │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .          │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .          │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .          │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .          │
    │ Page up and down in 10 element map                                      │ .                 │ .          │
    │ Page up and down in 100 element map                                     │ .                 │ .          │
    │ Page up and down in 101 element map                                     │ .                 │ .          │
    │ Page up and down in 1000 element map                                    │ .                 │ .          │
    │ Page up and down in 10000 element map                                   │ .                 │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +544 (102.45%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                 │ .          │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +544 (102.06%)    │ .          │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                 │ .          │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                 │ .          │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +1_224 (135.55%)  │ .          │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +1_224 (135.55%)  │ .          │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +1_224 (135.55%)  │ .          │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +6_664 (162.42%)  │ .          │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +6_664 (162.42%)  │ .          │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +13_464 (166.16%) │ .          │
    │ Invert ordering of 10 element map                                       │ .                 │ .          │
    │ Invert ordering of 100 element map                                      │ .                 │ .          │
    │ Invert ordering of 101 element map                                      │ .                 │ .          │
    │ Invert ordering of 1000 element map                                     │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                 │ .          │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                 │ .          │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                 │ .          │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                 │ .          │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴────────────┘
    |}]
;;
