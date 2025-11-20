open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

let title = "new - old"

(* This test compares the old vs new API for dynamic cells and columns. Ideally, we'd like
   these numbers to be negative or zero, since that means the new API requires fewer
   incremental nodes than the old one. *)

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
    │ dyn cells: 100          │ -15 (14%)  │ -5_252 (41%) │ -7_064 (46%) │ -7_064 (46%)  │
    │ dyn cols, no cf: 100    │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100       │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cells: 100000       │ -15 (14%)  │ -5_304 (41%) │ -7_134 (46%) │ -7_134 (46%)  │
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
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -572 (36%)   │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -5_252 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -5_304 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -5_304 (41%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -5_304 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -572 (36%)   │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -5_252 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ -5_304 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ -5_304 (41%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ -5_304 (41%) │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ -572 (36%)   │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ -5_252 (41%) │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ -5_304 (41%) │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ -5_304 (41%) │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ -5_304 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ -572 (36%)   │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ -5_252 (41%) │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ -5_304 (41%) │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ -5_304 (41%) │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ -5_304 (41%) │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ -572 (36%)   │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ -5_252 (41%) │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ -5_304 (41%) │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ -5_304 (41%) │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ -5_304 (41%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -104 (23%)   │ +3 (1%)         │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -572 (36%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -104 (23%)   │ +3 (1%)         │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -572 (36%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -5_252 (41%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -572 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -572 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -572 (36%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -2_652 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -2_652 (40%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -5_252 (41%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ -572 (36%)   │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ -5_252 (41%) │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ -5_304 (41%) │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ -5_304 (41%) │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ -572 (36%)   │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ -572 (36%)   │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -572 (36%)   │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ -572 (36%)   │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ -572 (36%)   │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -572 (36%)   │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -572 (36%)   │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -5_252 (41%) │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -352 (26%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -1_120 (46%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -352 (26%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -1_120 (46%) │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -1_120 (46%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -8_712 (26%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ .            │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .            │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells     │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .             │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .             │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .             │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .             │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .             │ +2 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .             │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .             │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .             │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .             │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .             │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .             │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .             │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .             │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .             │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .             │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .             │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .             │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .             │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .             │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .             │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .             │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -1_512 (39%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -1_512 (39%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -1_512 (39%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -1_512 (39%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -1_512 (39%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -2_128 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -2_128 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -2_128 (41%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -10_448 (41%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -10_448 (41%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -20_848 (42%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ -246 (66%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ -2_406 (83%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ -2_430 (83%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ -2_430 (83%)  │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .             │ .               │ .            │
    │ cell in it.                                                                              │               │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .             │ .               │ .            │
    │ cell in it.                                                                              │               │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .             │ .               │ .            │
    │ one cell in it.                                                                          │               │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .             │ .               │ .            │
    │ cells in it.                                                                             │               │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .             │ .               │ .            │
    │ cells in it.                                                                             │               │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .             │ .               │ .            │
    │ all cells in it.                                                                         │               │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -1_160 (49%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -1_160 (49%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -1_160 (49%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -1_160 (49%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -2_600 (54%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -8_000 (57%)  │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -946 (44%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -636 (73%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -952 (44%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -642 (73%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -102 (63%)   │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -792 (25%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -4_312 (26%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -8_712 (26%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ .            │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .            │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘
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
    │ dyn cells: 100          │ -15 (14%)  │ -4_359 (39%) │ -5_879 (44%) │ -5_878 (44%)  │
    │ dyn cols, no cf: 100    │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cols, cf: 100       │ -1 (1%)    │ .            │ .            │ .             │
    │ dyn cells: 100000       │ -15 (14%)  │ -4_402 (39%) │ -5_937 (44%) │ -5_936 (44%)  │
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
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -489 (34%)   │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -4_359 (39%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -4_402 (39%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -4_402 (39%) │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -4_402 (39%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ -489 (34%)   │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ -4_359 (39%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ -4_402 (39%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ -4_402 (39%) │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ -4_402 (39%) │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ -489 (34%)   │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ -4_359 (39%) │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ -4_402 (39%) │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ -4_402 (39%) │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ -4_402 (39%) │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ -489 (34%)   │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ -4_359 (39%) │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ -4_402 (39%) │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ -4_402 (39%) │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ -4_402 (39%) │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ -489 (34%)   │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ -4_359 (39%) │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ -4_402 (39%) │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ -4_402 (39%) │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ -4_402 (39%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -102 (23%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -489 (34%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -102 (23%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -489 (34%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -4_359 (39%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -489 (34%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -489 (34%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -489 (34%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -2_209 (38%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -2_209 (38%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -4_359 (39%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ -489 (34%)   │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ -4_359 (39%) │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ -4_402 (39%) │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ -4_402 (39%) │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ -489 (34%)   │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ -489 (34%)   │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -489 (34%)   │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ -489 (34%)   │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ -489 (34%)   │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ -489 (34%)   │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -489 (34%)   │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -4_359 (39%) │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -288 (25%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -928 (44%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -288 (25%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -928 (44%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -928 (44%)   │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -7_128 (25%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ .            │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .            │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬───────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells     │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼───────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ -2 (1%)       │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ -2 (1%)       │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ -2 (1%)       │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ -2 (1%)       │ +2 (1%)         │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ -2 (1%)       │ +2 (1%)         │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .             │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .             │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .             │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .             │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .             │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .             │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .             │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .             │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .             │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .             │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .             │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .             │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .             │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .             │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .             │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .             │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .             │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -1_370 (38%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -1_370 (38%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -1_370 (38%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -1_370 (38%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -1_370 (38%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -1_810 (39%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -1_810 (39%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -1_810 (39%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -8_690 (40%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -8_690 (40%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -17_290 (40%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ -274 (68%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ -2_524 (84%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ -2_549 (84%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ -2_549 (84%)  │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .             │ .               │ .            │
    │ cell in it.                                                                              │               │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .             │ .               │ .            │
    │ cell in it.                                                                              │               │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .             │ .               │ .            │
    │ one cell in it.                                                                          │               │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .             │ .               │ .            │
    │ cells in it.                                                                             │               │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .             │ .               │ .            │
    │ cells in it.                                                                             │               │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .             │ .               │ .            │
    │ all cells in it.                                                                         │               │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ -1_029 (47%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ -2_217 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ -1_029 (47%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ -2_217 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ -1_029 (47%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ -2_217 (51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ -1_029 (47%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ -2_217 (51%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ -6_672 (54%)  │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴───────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌──────────────────────────────────────────────────────────────────────────────────────────┬──────────────┬─────────────────┬──────────────┐
    │                                                                                          │ dyn cells    │ dyn cols, no cf │ dyn cols, cf │
    ├──────────────────────────────────────────────────────────────────────────────────────────┼──────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map                             │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map                            │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map                           │ .            │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map                          │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                                 │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map                                │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map                               │ .            │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map                              │ .            │ .               │ .            │
    │ Focus up and down in 10 element map                                                      │ .            │ .               │ .            │
    │ Focus up and down in 100 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 101 element map                                                     │ .            │ .               │ .            │
    │ Focus up and down in 1000 element map                                                    │ .            │ .               │ .            │
    │ Focus up and down in 10000 element map                                                   │ .            │ .               │ .            │
    │ Focus left and right in a map with 10 rows                                               │ .            │ .               │ .            │
    │ Focus left and right in a map with 100 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 101 rows                                              │ .            │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                                             │ .            │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                                            │ .            │ .               │ .            │
    │ Page up and down in 10 element map                                                       │ .            │ .               │ .            │
    │ Page up and down in 100 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 101 element map                                                      │ .            │ .               │ .            │
    │ Page up and down in 1000 element map                                                     │ .            │ .               │ .            │
    │ Page up and down in 10000 element map                                                    │ .            │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map                             │ -783 (42%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map                            │ -530 (69%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map                            │ -788 (42%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map                           │ -535 (69%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map                          │ -85 (59%)    │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window                           │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window                           │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window                          │ -648 (23%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window                          │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window                         │ -3_528 (25%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window                        │ -7_128 (25%) │ .               │ .            │
    │ Invert ordering of 10 element map                                                        │ .            │ .               │ .            │
    │ Invert ordering of 100 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 101 element map                                                       │ .            │ .               │ .            │
    │ Invert ordering of 1000 element map                                                      │ .            │ .               │ .            │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change one    │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change one   │ .            │ .               │ .            │
    │ cell in it.                                                                              │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ one cell in it.                                                                          │              │                 │              │
    │ Randomly select a row out of a table with 10 rows and a window of 10, then change all    │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 100 rows and a window of 10, then change all   │ .            │ .               │ .            │
    │ cells in it.                                                                             │              │                 │              │
    │ Randomly select a row out of a table with 10000 rows and a window of 10, then change     │ .            │ .               │ .            │
    │ all cells in it.                                                                         │              │                 │              │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window                       │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window                      │ .            │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window                     │ .            │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window                   │ .            │ .               │ .            │
    └──────────────────────────────────────────────────────────────────────────────────────────┴──────────────┴─────────────────┴──────────────┘
    |}]
;;
