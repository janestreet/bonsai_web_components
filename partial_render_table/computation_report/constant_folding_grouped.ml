open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "grouped - flat"

(* This test compares constant folding of configs with / without column grouping.
   We would expect the results to be slightly lower, but not by a ton.
   So the numbers should be positive, but not too huge.  *)

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
    │ new api, counters, static: 100       │ .          │ +5 (0.47%) │ +7 (0.46%)  │ +5 (0.33%)    │
    │ new api, no counters, static: 100    │ .          │ +5 (0.47%) │ +7 (0.47%)  │ +5 (0.33%)    │
    │ new api, counters: 100000            │ .          │ .          │ .           │ .             │
    │ new api, no counters: 100000         │ .          │ .          │ .           │ .             │
    │ new api, counters, static: 100000    │ .          │ +5 (0.46%) │ +7 (0.46%)  │ +5 (0.33%)    │
    │ new api, no counters, static: 100000 │ .          │ +5 (0.47%) │ +7 (0.46%)  │ +5 (0.33%)    │
    └──────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘
    |}];
  test_startup (configs Stateful_rows);
  [%expect
    {|
    ======= Startup Incr Node Stats (grouped - flat) =======
    ┌──────────────────────────────────────┬────────────┬──────────────┬──────────────┬───────────────┐
    │                                      │ max_height │ node_count   │ max_node_id  │ nodes_created │
    ├──────────────────────────────────────┼────────────┼──────────────┼──────────────┼───────────────┤
    │ new api, counters: 100               │ .          │ .            │ .            │ .             │
    │ new api, no counters: 100            │ .          │ .            │ .            │ .             │
    │ new api, counters, static: 100       │ .          │ -95 (-2.59%) │ -93 (-2.11%) │ -95 (-2.16%)  │
    │ new api, no counters, static: 100    │ .          │ +5 (0.47%)   │ +7 (0.47%)   │ +5 (0.33%)    │
    │ new api, counters: 100000            │ .          │ .            │ .            │ .             │
    │ new api, no counters: 100000         │ .          │ .            │ .            │ .             │
    │ new api, counters, static: 100000    │ .          │ -96 (-2.60%) │ -94 (-2.12%) │ -96 (-2.16%)  │
    │ new api, no counters, static: 100000 │ .          │ +5 (0.47%)   │ +7 (0.46%)   │ +5 (0.33%)    │
    └──────────────────────────────────────┴────────────┴──────────────┴──────────────┴───────────────┘
    |}];
  test_startup (configs Stateful_cells);
  [%expect
    {|
    ======= Startup Incr Node Stats (grouped - flat) =======
    ┌──────────────────────────────────────┬────────────┬────────────────┬───────────────┬───────────────┐
    │                                      │ max_height │ node_count     │ max_node_id   │ nodes_created │
    ├──────────────────────────────────────┼────────────┼────────────────┼───────────────┼───────────────┤
    │ new api, counters: 100               │ .          │ -800 (-9.92%)  │ -800 (-9.09%) │ -800 (-9.09%) │
    │ new api, no counters: 100            │ .          │ .              │ .             │ .             │
    │ new api, counters, static: 100       │ .          │ -795 (-10.11%) │ -793 (-9.22%) │ -795 (-9.24%) │
    │ new api, no counters, static: 100    │ .          │ +5 (0.47%)     │ +7 (0.47%)    │ +5 (0.33%)    │
    │ new api, counters: 100000            │ .          │ -808 (-9.93%)  │ -808 (-9.09%) │ -808 (-9.09%) │
    │ new api, no counters: 100000         │ .          │ .              │ .             │ .             │
    │ new api, counters, static: 100000    │ .          │ -803 (-10.11%) │ -801 (-9.22%) │ -803 (-9.25%) │
    │ new api, no counters, static: 100000 │ .          │ +5 (0.47%)     │ +7 (0.46%)    │ +5 (0.33%)    │
    └──────────────────────────────────────┴────────────┴────────────────┴───────────────┴───────────────┘
    |}]
;;
