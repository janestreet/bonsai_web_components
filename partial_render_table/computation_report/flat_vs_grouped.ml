open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "Grouped - Flat"

(* This test compares flat vs grouped column structures. We expect the grouped tests to
   be a little slower / have a few more nodes than the flat ones, but not by a ton.
   So these numbers will probably be positive, but shouldn't be too huge. *)

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

let%expect_test "Flat -> Grouped" =
  let configs =
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
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (Grouped - Flat) =======
    ┌───────────────────────────────────┬────────────┬─────────────────┬─────────────────┬─────────────────┐
    │                                   │ max_height │ node_count      │ max_node_id     │ nodes_created   │
    ├───────────────────────────────────┼────────────┼─────────────────┼─────────────────┼─────────────────┤
    │ dyn cells, counters: 100          │ .          │ +1_785 (14.95%) │ +2_075 (14.62%) │ +2_078 (14.64%) │
    │ dyn cells, no counters: 100       │ .          │ -19 (-1.71%)    │ -31 (-1.99%)    │ -28 (-1.80%)    │
    │ dyn cols, counters: 100           │ .          │ .               │ .               │ .               │
    │ dyn cols, no counters, cf: 100    │ .          │ .               │ .               │ .               │
    │ dyn cells, counters: 100000       │ .          │ +1_803 (14.96%) │ +2_096 (14.62%) │ +2_099 (14.65%) │
    │ dyn cells, no counters: 100000    │ .          │ -19 (-1.70%)    │ -31 (-1.97%)    │ -28 (-1.78%)    │
    │ dyn cols, counters: 100000        │ .          │ .               │ .               │ .               │
    │ dyn cols, no counters, cf: 100000 │ .          │ .               │ .               │ .               │
    └───────────────────────────────────┴────────────┴─────────────────┴─────────────────┴─────────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (Grouped - Flat) ======
    ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                         │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├─────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map               │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map               │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus up and down in 10 element map                                     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Focus up and down in 100 element map                                    │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Focus up and down in 101 element map                                    │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                   │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                  │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                              │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                             │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                             │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                            │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                           │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Page up and down in 10 element map                                      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Page up and down in 100 element map                                     │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Page up and down in 101 element map                                     │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Page up and down in 1000 element map                                    │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Page up and down in 10000 element map                                   │ +1_803 (14.96%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +3 (0.66%)          │ -19 (-5.86%)           │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +165 (11.00%)       │ -19 (-4.80%)           │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +3 (0.66%)          │ -19 (-5.86%)           │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +165 (11.00%)       │ -19 (-4.80%)           │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +1_785 (14.95%)     │ -19 (-1.70%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +165 (11.02%)       │ -19 (-4.83%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +165 (11.02%)       │ -19 (-4.83%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +165 (11.02%)       │ -19 (-4.83%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +885 (14.42%)       │ -19 (-2.66%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +885 (14.42%)       │ -19 (-2.66%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Invert ordering of 10 element map                                       │ +165 (11.02%)       │ -19 (-4.83%)           │ .                  │ .                         │
    │ Invert ordering of 100 element map                                      │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    │ Invert ordering of 101 element map                                      │ +1_803 (14.96%)     │ -19 (-1.69%)           │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                     │ +1_803 (14.96%)     │ -19 (-1.69%)           │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +165 (11.03%)       │ -19 (-4.85%)           │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +1_785 (14.95%)     │ -19 (-1.71%)           │ .                  │ .                         │
    └─────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Created (Grouped - Flat) ======
    ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                         │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├─────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 100 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 101 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                   │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                  │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                           │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 100 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 101 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 1000 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10000 element map                                   │ .                   │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +208 (16.44%)       │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +336 (15.10%)       │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +208 (16.44%)       │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +336 (15.10%)       │ .                      │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +336 (15.10%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +468 (15.62%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +468 (15.62%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +468 (15.62%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +2_548 (16.46%)     │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +2_548 (16.46%)     │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +5_148 (16.57%)     │ .                      │ .                  │ .                         │
    │ Invert ordering of 10 element map                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 100 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 101 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                   │ .                      │ .                  │ .                         │
    └─────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Recomputed (Grouped - Flat) ======
    ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                         │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├─────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ +4 (7.69%)          │ +4 (7.69%)             │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +4 (7.69%)          │ +4 (7.69%)             │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +4 (7.69%)          │ +4 (7.69%)             │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +4 (7.69%)          │ +4 (7.69%)             │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +4 (7.69%)          │ +4 (7.69%)             │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                │ +4 (3.28%)          │ +4 (3.28%)             │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map               │ +4 (1.32%)          │ +4 (1.32%)             │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map               │ +4 (1.32%)          │ +4 (1.32%)             │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +4 (1.32%)          │ +4 (1.32%)             │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +4 (1.32%)          │ +4 (1.32%)             │ .                  │ .                         │
    │ Focus up and down in 10 element map                                     │ +2 (3.28%)          │ +2 (3.28%)             │ .                  │ .                         │
    │ Focus up and down in 100 element map                                    │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus up and down in 101 element map                                    │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                   │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                  │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                              │ +2 (3.28%)          │ +2 (3.28%)             │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                             │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                             │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                            │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                           │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Page up and down in 10 element map                                      │ +2 (3.28%)          │ +2 (3.28%)             │ .                  │ .                         │
    │ Page up and down in 100 element map                                     │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Page up and down in 101 element map                                     │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Page up and down in 1000 element map                                    │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Page up and down in 10000 element map                                   │ +2 (1.32%)          │ +2 (1.32%)             │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +288 (7.45%)        │ -17 (-0.84%)           │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +288 (7.42%)        │ -17 (-0.83%)           │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +288 (7.45%)        │ -17 (-0.84%)           │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +288 (7.42%)        │ -17 (-0.83%)           │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +288 (7.42%)        │ -17 (-0.83%)           │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +640 (13.30%)       │ -4 (-0.50%)            │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +640 (13.30%)       │ -4 (-0.50%)            │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +640 (13.30%)       │ -4 (-0.50%)            │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +3_520 (15.16%)     │ -4 (-0.21%)            │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +3_520 (15.16%)     │ -4 (-0.21%)            │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +7_120 (15.41%)     │ -4 (-0.12%)            │ .                  │ .                         │
    │ Invert ordering of 10 element map                                       │ -38 (-9.16%)        │ -1 (-0.52%)            │ .                  │ .                         │
    │ Invert ordering of 100 element map                                      │ -218 (-7.00%)       │ -1 (-0.14%)            │ .                  │ .                         │
    │ Invert ordering of 101 element map                                      │ -220 (-7.00%)       │ -1 (-0.14%)            │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                     │ -220 (-7.00%)       │ -1 (-0.14%)            │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ +171 (7.99%)        │ -10 (-0.95%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ +495 (11.51%)       │ -10 (-0.79%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ +171 (7.99%)        │ -10 (-0.95%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ +495 (11.51%)       │ -10 (-0.79%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ +171 (7.99%)        │ -10 (-0.95%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ +495 (11.51%)       │ -10 (-0.79%)           │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ +171 (7.99%)        │ -10 (-0.95%)           │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ +495 (11.51%)       │ -10 (-0.79%)           │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ +1_710 (13.79%)     │ -10 (-0.48%)           │ .                  │ .                         │
    └─────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘

    ====== Nodes Invalidated (Grouped - Flat) ======
    ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────┬────────────────────────┬────────────────────┬───────────────────────────┐
    │                                                                         │ dyn cells, counters │ dyn cells, no counters │ dyn cols, counters │ dyn cols, no counters, cf │
    ├─────────────────────────────────────────────────────────────────────────┼─────────────────────┼────────────────────────┼────────────────────┼───────────────────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                   │ .                      │ .                  │ .                         │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 100 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 101 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 1000 element map                                   │ .                   │ .                      │ .                  │ .                         │
    │ Focus up and down in 10000 element map                                  │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10 rows                              │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 100 rows                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 101 rows                             │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 1000 rows                            │ .                   │ .                      │ .                  │ .                         │
    │ Focus left and right in a map with 10000 rows                           │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 100 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 101 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 1000 element map                                    │ .                   │ .                      │ .                  │ .                         │
    │ Page up and down in 10000 element map                                   │ .                   │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ +291 (15.66%)       │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ +106 (14.04%)       │ .                      │ .                  │ .                         │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ +292 (15.66%)       │ .                      │ .                  │ .                         │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ +107 (14.04%)       │ .                      │ .                  │ .                         │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ +17 (12.88%)        │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ +432 (15.57%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ +432 (15.57%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ +432 (15.57%)       │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ +2_352 (16.45%)     │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ +2_352 (16.45%)     │ .                      │ .                  │ .                         │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ +4_752 (16.56%)     │ .                      │ .                  │ .                         │
    │ Invert ordering of 10 element map                                       │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 100 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 101 element map                                      │ .                   │ .                      │ .                  │ .                         │
    │ Invert ordering of 1000 element map                                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change one cell in it.                      │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Randomly select a row, then change all cells in it.                     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                   │ .                      │ .                  │ .                         │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                   │ .                      │ .                  │ .                         │
    └─────────────────────────────────────────────────────────────────────────┴─────────────────────┴────────────────────────┴────────────────────┴───────────────────────────┘
    |}]
;;
