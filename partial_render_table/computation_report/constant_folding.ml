open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "foldable - not foldable"

(* This test compares "full-power" vs constant-foldable structures. We expect the constant
foldable ones to be significantly smaller, so these numbers should all be negative. *)

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

let%expect_test "Constant folding cols" =
  let configs =
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
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (foldable - not foldable) =======
    ┌──────────────────────────────┬───────────────┬──────────────────┬──────────────────┬──────────────────┐
    │                              │ max_height    │ node_count       │ max_node_id      │ nodes_created    │
    ├──────────────────────────────┼───────────────┼──────────────────┼──────────────────┼──────────────────┤
    │ new api, counters: 100       │ +1 (1.22%)    │ -305 (-3.64%)    │ -2_606 (-22.85%) │ -2_605 (-22.84%) │
    │ new api, no counters: 100    │ -13 (-17.11%) │ -3_109 (-74.59%) │ -7_110 (-82.62%) │ -7_111 (-82.63%) │
    │ dyn cols, counters: 100      │ .             │ -2 (-0.19%)      │ -3 (-0.20%)      │ -2 (-0.13%)      │
    │ new api, counters: 100000    │ +1 (1.22%)    │ -308 (-3.65%)    │ -2_632 (-22.85%) │ -2_631 (-22.84%) │
    │ new api, no counters: 100000 │ -13 (-17.11%) │ -3_140 (-74.64%) │ -7_181 (-82.64%) │ -7_182 (-82.66%) │
    │ dyn cols, counters: 100000   │ .             │ -2 (-0.19%)      │ -3 (-0.20%)      │ -2 (-0.13%)      │
    └──────────────────────────────┴───────────────┴──────────────────┴──────────────────┴──────────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (foldable - not foldable) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                         │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus by key (key present) and unfocus in 10 element map                │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Focus by key (key present) and unfocus in 100 element map               │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    │ Focus by key (key present) and unfocus in 101 element map               │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus up and down in 10 element map                                     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Focus up and down in 100 element map                                    │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    │ Focus up and down in 101 element map                                    │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus up and down in 1000 element map                                   │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus up and down in 10000 element map                                  │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus left and right in a map with 10 rows                              │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Focus left and right in a map with 100 rows                             │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    │ Focus left and right in a map with 101 rows                             │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus left and right in a map with 1000 rows                            │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Focus left and right in a map with 10000 rows                           │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Page up and down in 10 element map                                      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Page up and down in 100 element map                                     │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    │ Page up and down in 101 element map                                     │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Page up and down in 1000 element map                                    │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Page up and down in 10000 element map                                   │ -308 (-3.65%)     │ -3_140 (-74.64%)     │ -2 (-0.19%)        │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -8 (-2.27%)       │ -40 (-12.86%)        │ -2 (-0.72%)        │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -35 (-3.23%)      │ -319 (-48.19%)       │ -2 (-0.57%)        │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -8 (-2.27%)       │ -40 (-12.86%)        │ -2 (-0.72%)        │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -35 (-3.23%)      │ -319 (-48.19%)       │ -2 (-0.57%)        │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -305 (-3.64%)     │ -3_109 (-74.52%)     │ -2 (-0.19%)        │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -35 (-3.24%)      │ -319 (-48.41%)       │ -2 (-0.57%)        │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -35 (-3.24%)      │ -319 (-48.41%)       │ -2 (-0.57%)        │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -35 (-3.24%)      │ -319 (-48.41%)       │ -2 (-0.57%)        │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -155 (-3.59%)     │ -1_559 (-70.26%)     │ -2 (-0.30%)        │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -155 (-3.59%)     │ -1_559 (-70.26%)     │ -2 (-0.30%)        │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -305 (-3.64%)     │ -3_109 (-74.57%)     │ -2 (-0.19%)        │
    │ Invert ordering of 10 element map                                       │ -35 (-3.24%)      │ -319 (-48.41%)       │ -2 (-0.57%)        │
    │ Invert ordering of 100 element map                                      │ -305 (-3.64%)     │ -3_109 (-74.57%)     │ -2 (-0.19%)        │
    │ Invert ordering of 101 element map                                      │ -308 (-3.64%)     │ -3_140 (-74.62%)     │ -2 (-0.19%)        │
    │ Invert ordering of 1000 element map                                     │ -308 (-3.64%)     │ -3_140 (-74.62%)     │ -2 (-0.19%)        │
    │ Randomly select a row, then change one cell in it.                      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Randomly select a row, then change one cell in it.                      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Randomly select a row, then change one cell in it.                      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Randomly select a row, then change all cells in it.                     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Randomly select a row, then change all cells in it.                     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Randomly select a row, then change all cells in it.                     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -35 (-3.25%)      │ -319 (-48.48%)       │ -2 (-0.58%)        │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -305 (-3.64%)     │ -3_109 (-74.59%)     │ -2 (-0.19%)        │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Created (foldable - not foldable) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                         │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .                    │ .                  │
    │ Focus up and down in 10 element map                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 100 element map                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 101 element map                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 1000 element map                                   │ .                 │ .                    │ .                  │
    │ Focus up and down in 10000 element map                                  │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .                    │ .                  │
    │ Page up and down in 10 element map                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 100 element map                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 101 element map                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 1000 element map                                    │ .                 │ .                    │ .                  │
    │ Page up and down in 10000 element map                                   │ .                 │ .                    │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -416 (-28.24%)    │ -1_008 (-98.34%)     │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -416 (-23.20%)    │ -1_136 (-84.46%)     │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -416 (-28.24%)    │ -1_008 (-98.34%)     │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -416 (-23.20%)    │ -1_136 (-84.46%)     │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -416 (-23.20%)    │ -1_136 (-84.46%)     │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -936 (-27.02%)    │ -2_268 (-92.35%)     │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -936 (-27.02%)    │ -2_268 (-92.35%)     │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -936 (-27.02%)    │ -2_268 (-92.35%)     │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -5_096 (-28.27%)  │ -12_348 (-98.50%)    │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -5_096 (-28.27%)  │ -12_348 (-98.50%)    │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -10_296 (-28.42%) │ -24_948 (-99.25%)    │ .                  │
    │ Invert ordering of 10 element map                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 100 element map                                      │ .                 │ .                    │ .                  │
    │ Invert ordering of 101 element map                                      │ .                 │ .                    │ .                  │
    │ Invert ordering of 1000 element map                                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                 │ .                    │ .                  │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Recomputed (foldable - not foldable) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                         │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .                    │ +4 (7.69%)         │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .                    │ +4 (7.69%)         │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .                    │ +4 (7.69%)         │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .                    │ +4 (7.69%)         │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .                    │ +4 (7.69%)         │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .                    │ +4 (3.28%)         │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .                    │ +4 (1.32%)         │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .                    │ +4 (1.32%)         │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .                    │ +4 (1.32%)         │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .                    │ +4 (1.32%)         │
    │ Focus up and down in 10 element map                                     │ .                 │ .                    │ +2 (3.28%)         │
    │ Focus up and down in 100 element map                                    │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus up and down in 101 element map                                    │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus up and down in 1000 element map                                   │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus up and down in 10000 element map                                  │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .                    │ +2 (3.28%)         │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .                    │ +2 (1.32%)         │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .                    │ +2 (1.32%)         │
    │ Page up and down in 10 element map                                      │ .                 │ .                    │ +2 (3.28%)         │
    │ Page up and down in 100 element map                                     │ .                 │ .                    │ +2 (1.32%)         │
    │ Page up and down in 101 element map                                     │ .                 │ .                    │ +2 (1.32%)         │
    │ Page up and down in 1000 element map                                    │ .                 │ .                    │ +2 (1.32%)         │
    │ Page up and down in 10000 element map                                   │ .                 │ .                    │ +2 (1.32%)         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -48 (-1.83%)      │ -513 (-26.27%)       │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -48 (-1.82%)      │ -513 (-26.05%)       │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -48 (-1.83%)      │ -513 (-26.27%)       │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -48 (-1.82%)      │ -513 (-26.05%)       │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -48 (-1.82%)      │ -513 (-26.05%)       │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -108 (-3.28%)     │ -1_112 (-62.61%)     │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -108 (-3.28%)     │ -1_112 (-62.61%)     │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -108 (-3.28%)     │ -1_112 (-62.61%)     │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -588 (-3.65%)     │ -6_072 (-77.29%)     │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -588 (-3.65%)     │ -6_072 (-77.29%)     │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -1_188 (-3.70%)   │ -12_272 (-79.40%)    │ .                  │
    │ Invert ordering of 10 element map                                       │ .                 │ +25 (19.08%)         │ .                  │
    │ Invert ordering of 100 element map                                      │ .                 │ +205 (41.75%)        │ .                  │
    │ Invert ordering of 101 element map                                      │ .                 │ +207 (41.82%)        │ .                  │
    │ Invert ordering of 1000 element map                                     │ .                 │ +207 (41.82%)        │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +152 (14.66%)     │ -333 (-32.11%)       │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +440 (23.59%)     │ -945 (-50.67%)       │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +152 (14.66%)     │ -333 (-32.11%)       │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +440 (23.59%)     │ -945 (-50.67%)       │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +152 (14.66%)     │ -333 (-32.11%)       │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +440 (23.59%)     │ -945 (-50.67%)       │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +152 (14.66%)     │ -333 (-32.11%)       │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +440 (23.59%)     │ -945 (-50.67%)       │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +1_520 (30.58%)   │ -3_240 (-65.19%)     │ .                  │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘

    ====== Nodes Invalidated (foldable - not foldable) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬──────────────────────┬────────────────────┐
    │                                                                         │ new api, counters │ new api, no counters │ dyn cols, counters │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼──────────────────────┼────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .                    │ .                  │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .                    │ .                  │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .                    │ .                  │
    │ Focus up and down in 10 element map                                     │ .                 │ .                    │ .                  │
    │ Focus up and down in 100 element map                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 101 element map                                    │ .                 │ .                    │ .                  │
    │ Focus up and down in 1000 element map                                   │ .                 │ .                    │ .                  │
    │ Focus up and down in 10000 element map                                  │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .                    │ .                  │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .                    │ .                  │
    │ Page up and down in 10 element map                                      │ .                 │ .                    │ .                  │
    │ Page up and down in 100 element map                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 101 element map                                     │ .                 │ .                    │ .                  │
    │ Page up and down in 1000 element map                                    │ .                 │ .                    │ .                  │
    │ Page up and down in 10000 element map                                   │ .                 │ .                    │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +704 (132.58%)    │ -419 (-78.91%)       │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ .                 │ -106 (-47.11%)       │ .                  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +704 (132.08%)    │ -420 (-78.80%)       │ .                  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ .                 │ -107 (-47.14%)       │ .                  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ .                 │ -17 (-36.17%)        │ .                  │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +1_584 (175.42%)  │ -720 (-79.73%)       │ .                  │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +1_584 (175.42%)  │ -720 (-79.73%)       │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +1_584 (175.42%)  │ -720 (-79.73%)       │ .                  │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +8_624 (210.19%)  │ -3_920 (-95.54%)     │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +8_624 (210.19%)  │ -3_920 (-95.54%)     │ .                  │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +17_424 (215.03%) │ -7_920 (-97.74%)     │ .                  │
    │ Invert ordering of 10 element map                                       │ .                 │ .                    │ .                  │
    │ Invert ordering of 100 element map                                      │ .                 │ .                    │ .                  │
    │ Invert ordering of 101 element map                                      │ .                 │ .                    │ .                  │
    │ Invert ordering of 1000 element map                                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                 │ .                    │ .                  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                 │ .                    │ .                  │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴──────────────────────┴────────────────────┘
    |}]
;;
