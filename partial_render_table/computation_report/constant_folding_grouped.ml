open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "grouped - flat"

(* This test compares constant folding of configs with / without column grouping. We would
   expect the results to be slightly lower, but not by a ton. So the numbers should be
   positive, but not too huge. *)

let test_startup pairs =
  Report.Startup.diff_pairs_incr_summary_only
    ~title
    ~computation_pairs:(List.map pairs ~f:Config.pair_for_diff)
    (Symbol_table.startup_inputs [ 100; 100_000 ])
;;

let%expect_test "Not Grouped vs Grouped" =
  let configs render_cell_kind =
    [ ( "new api, counters"
      , Config.New_api
          { counters_in_cells = true
          ; cols = Dynamic_constant_foldable
          ; col_groups = false
          ; render_cell_kind
          ; duplicate_col = false
          }
      , Config.New_api
          { counters_in_cells = true
          ; cols = Dynamic_constant_foldable
          ; col_groups = true
          ; render_cell_kind
          ; duplicate_col = false
          } )
    ; ( "new api, no counters"
      , Config.New_api
          { counters_in_cells = false
          ; cols = Dynamic_constant_foldable
          ; col_groups = false
          ; render_cell_kind
          ; duplicate_col = false
          }
      , Config.New_api
          { counters_in_cells = false
          ; cols = Dynamic_constant_foldable
          ; col_groups = true
          ; render_cell_kind
          ; duplicate_col = false
          } )
    ; ( "new api, counters, static"
      , Config.New_api
          { counters_in_cells = true
          ; cols = Static
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
    ; ( "new api, no counters, static"
      , Config.New_api
          { counters_in_cells = false
          ; cols = Static
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
  in
  test_startup (configs Pure);
  [%expect
    {|
    ======= Startup Incr Node Stats (grouped - flat) =======
    ┌──────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                                      │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ new api, counters: 100               │ .          │ .          │ .           │ .             │
    │ new api, no counters: 100            │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100       │ .          │ .          │ .           │ .             │
    │ new api, no counters, static: 100    │ .          │ .          │ .           │ .             │
    │ new api, counters: 100000            │ .          │ .          │ .           │ .             │
    │ new api, no counters: 100000         │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100000    │ .          │ .          │ .           │ .             │
    │ new api, no counters, static: 100000 │ .          │ .          │ .           │ .             │
    └──────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  test_startup (configs Stateful_rows);
  [%expect
    {|
    ======= Startup Incr Node Stats (grouped - flat) =======
    ┌──────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                                      │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ new api, counters: 100               │ .          │ .          │ .           │ .             │
    │ new api, no counters: 100            │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100       │ .          │ -95 (3%)   │ -93 (2%)    │ -95 (2%)      │
    │ new api, no counters, static: 100    │ .          │ .          │ .           │ .             │
    │ new api, counters: 100000            │ .          │ .          │ .           │ .             │
    │ new api, no counters: 100000         │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100000    │ .          │ -96 (3%)   │ -94 (2%)    │ -96 (2%)      │
    │ new api, no counters, static: 100000 │ .          │ .          │ .           │ .             │
    └──────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  test_startup (configs Stateful_cells);
  [%expect
    {|
    ======= Startup Incr Node Stats (grouped - flat) =======
    ┌──────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
    │                                      │ max_height │ node_count │ max_node_id │ nodes_created │
    ├──────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
    │ new api, counters: 100               │ .          │ -800 (10%) │ -800 (9%)   │ -800 (9%)     │
    │ new api, no counters: 100            │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100       │ .          │ -795 (10%) │ -793 (9%)   │ -795 (9%)     │
    │ new api, no counters, static: 100    │ .          │ .          │ .           │ .             │
    │ new api, counters: 100000            │ .          │ -808 (10%) │ -808 (9%)   │ -808 (9%)     │
    │ new api, no counters: 100000         │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100000    │ .          │ -803 (10%) │ -801 (9%)   │ -803 (9%)     │
    │ new api, no counters, static: 100000 │ .          │ .          │ .           │ .             │
    └──────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}]
;;
