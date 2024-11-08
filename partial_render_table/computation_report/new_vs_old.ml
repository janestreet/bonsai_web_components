open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

let title = "new - old"

(* This test compares the old vs new API for dynamic cells and columns.
   Ideally, we'd like these numbers to be negative or zero, since that means the new API
   requires fewer incremental nodes than the old one. *)

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

let configs ~col_groups =
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
  let configs = configs ~col_groups:false in
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (new - old) =======
    ┌─────────────────────────┬───────────────┬──────────────────┬──────────────────┬──────────────────┐
    │                         │ max_height    │ node_count       │ max_node_id      │ nodes_created    │
    ├─────────────────────────┼───────────────┼──────────────────┼──────────────────┼──────────────────┤
    │ dyn cells: 100          │ -15 (-15.46%) │ -5_858 (-42.69%) │ -7_670 (-47.14%) │ -7_670 (-47.14%) │
    │ dyn cols, no cf: 100    │ -1 (-1.47%)   │ +3 (0.28%)       │ +5 (0.33%)       │ +3 (0.20%)       │
    │ dyn cols, cf: 100       │ -1 (-1.47%)   │ -1 (-0.09%)      │ +1 (0.07%)       │ -1 (-0.07%)      │
    │ dyn cells: 100000       │ -15 (-15.46%) │ -5_916 (-42.70%) │ -7_746 (-47.15%) │ -7_746 (-47.15%) │
    │ dyn cols, no cf: 100000 │ -1 (-1.47%)   │ +3 (0.28%)       │ +5 (0.33%)       │ +3 (0.20%)       │
    │ dyn cols, cf: 100000    │ -1 (-1.47%)   │ -1 (-0.09%)      │ +1 (0.07%)       │ -1 (-0.07%)      │
    └─────────────────────────┴───────────────┴──────────────────┴──────────────────┴──────────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells        │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 10 element map                │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Focus by key (key present) and unfocus in 100 element map               │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 101 element map               │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus up and down in 10 element map                                     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Focus up and down in 100 element map                                    │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus up and down in 101 element map                                    │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus up and down in 1000 element map                                   │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus up and down in 10000 element map                                  │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 10 rows                              │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Focus left and right in a map with 100 rows                             │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 101 rows                             │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 1000 rows                            │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 10000 rows                           │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Page up and down in 10 element map                                      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Page up and down in 100 element map                                     │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Page up and down in 101 element map                                     │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Page up and down in 1000 element map                                    │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Page up and down in 10000 element map                                   │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -116 (-25.27%)   │ +3 (1.08%)      │ -1 (-0.36%)  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -638 (-38.32%)   │ +3 (0.85%)      │ -1 (-0.29%)  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -116 (-25.27%)   │ +3 (1.08%)      │ -1 (-0.36%)  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -638 (-38.32%)   │ +3 (0.85%)      │ -1 (-0.29%)  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -5_858 (-42.68%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -638 (-38.39%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -638 (-38.39%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -638 (-38.39%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_958 (-42.12%) │ +3 (0.45%)      │ -1 (-0.15%)  │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_958 (-42.12%) │ +3 (0.45%)      │ -1 (-0.15%)  │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Invert ordering of 10 element map                                       │ -638 (-38.39%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Invert ordering of 100 element map                                      │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Invert ordering of 101 element map                                      │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Invert ordering of 1000 element map                                     │ -5_916 (-42.70%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    │ Randomly select a row, then change one cell in it.                      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change one cell in it.                      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change one cell in it.                      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -638 (-38.41%)   │ +3 (0.86%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -5_858 (-42.69%) │ +3 (0.28%)      │ -1 (-0.09%)  │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells         │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .                 │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .                 │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .                 │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .                 │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .                 │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .                 │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .                 │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .                 │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .                 │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .                 │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .                 │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .                 │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .                 │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -448 (-30.41%)    │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_216 (-47.48%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -448 (-30.41%)    │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_216 (-47.48%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_216 (-47.48%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_008 (-29.10%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_008 (-29.10%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_008 (-29.10%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -5_488 (-30.45%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -5_488 (-30.45%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -11_088 (-30.61%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .                 │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .                 │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .                 │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                 │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                 │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                 │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                 │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                 │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                 │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells         │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                 │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                 │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                 │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                 │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                 │ +4 (7.69%)      │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                 │ +4 (3.28%)      │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                 │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                 │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                 │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                 │ +4 (1.32%)      │ .            │
    │ Focus up and down in 10 element map                                     │ .                 │ +2 (3.28%)      │ .            │
    │ Focus up and down in 100 element map                                    │ .                 │ +2 (1.32%)      │ .            │
    │ Focus up and down in 101 element map                                    │ .                 │ +2 (1.32%)      │ .            │
    │ Focus up and down in 1000 element map                                   │ .                 │ +2 (1.32%)      │ .            │
    │ Focus up and down in 10000 element map                                  │ .                 │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 10 rows                              │ .                 │ +2 (3.28%)      │ .            │
    │ Focus left and right in a map with 100 rows                             │ .                 │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 101 rows                             │ .                 │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .                 │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .                 │ +2 (1.32%)      │ .            │
    │ Page up and down in 10 element map                                      │ .                 │ +2 (3.28%)      │ .            │
    │ Page up and down in 100 element map                                     │ .                 │ +2 (1.32%)      │ .            │
    │ Page up and down in 101 element map                                     │ .                 │ +2 (1.32%)      │ .            │
    │ Page up and down in 1000 element map                                    │ .                 │ +2 (1.32%)      │ .            │
    │ Page up and down in 10000 element map                                   │ .                 │ +2 (1.32%)      │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_608 (-38.72%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_608 (-38.57%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_608 (-38.72%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_608 (-38.57%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_608 (-38.57%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -2_344 (-42.99%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -2_344 (-42.99%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -2_344 (-42.99%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -11_624 (-43.48%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -11_624 (-43.48%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -23_224 (-43.55%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -246 (-65.25%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_406 (-83.05%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_430 (-83.08%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_430 (-83.08%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_160 (-50.19%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_600 (-54.22%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_160 (-50.19%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_600 (-54.22%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_160 (-50.19%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_600 (-54.22%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_160 (-50.19%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_600 (-54.22%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -8_000 (-56.70%)  │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells        │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .                │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .                │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .                │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .                │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .                │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .                │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .                │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -946 (-44.02%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -636 (-73.87%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -952 (-44.14%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -642 (-73.88%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -102 (-68.46%)   │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -792 (-24.70%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -792 (-24.70%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -792 (-24.70%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -4_312 (-25.90%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -4_312 (-25.90%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -8_712 (-26.05%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .                │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴──────────────┘
    |}]
;;

let%expect_test "Old -> New, col groups" =
  let configs = configs ~col_groups:true in
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats (new - old) =======
    ┌─────────────────────────┬───────────────┬──────────────────┬──────────────────┬──────────────────┐
    │                         │ max_height    │ node_count       │ max_node_id      │ nodes_created    │
    ├─────────────────────────┼───────────────┼──────────────────┼──────────────────┼──────────────────┤
    │ dyn cells: 100          │ -15 (-15.46%) │ -4_868 (-40.78%) │ -6_388 (-45.00%) │ -6_387 (-45.00%) │
    │ dyn cols, no cf: 100    │ -1 (-1.47%)   │ +2 (0.19%)       │ +4 (0.27%)       │ +2 (0.13%)       │
    │ dyn cols, cf: 100       │ -1 (-1.47%)   │ -1 (-0.09%)      │ +1 (0.07%)       │ -1 (-0.07%)      │
    │ dyn cells: 100000       │ -15 (-15.46%) │ -4_916 (-40.79%) │ -6_451 (-45.01%) │ -6_450 (-45.01%) │
    │ dyn cols, no cf: 100000 │ -1 (-1.47%)   │ +2 (0.19%)       │ +4 (0.26%)       │ +2 (0.13%)       │
    │ dyn cols, cf: 100000    │ -1 (-1.47%)   │ -1 (-0.09%)      │ +1 (0.07%)       │ -1 (-0.07%)      │
    └─────────────────────────┴───────────────┴──────────────────┴──────────────────┴──────────────────┘
    |}];
  Report.Interaction.diff_pairs ~title (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells        │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Focus by key (key not present) and unfocus in 100 element map           │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 101 element map           │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 10 element map                │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Focus by key (key present) and unfocus in 100 element map               │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 101 element map               │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 1000 element map              │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus by key (key present) and unfocus in 10000 element map             │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus up and down in 10 element map                                     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Focus up and down in 100 element map                                    │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus up and down in 101 element map                                    │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus up and down in 1000 element map                                   │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus up and down in 10000 element map                                  │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 10 rows                              │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Focus left and right in a map with 100 rows                             │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 101 rows                             │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 1000 rows                            │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Focus left and right in a map with 10000 rows                           │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Page up and down in 10 element map                                      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Page up and down in 100 element map                                     │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Page up and down in 101 element map                                     │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Page up and down in 1000 element map                                    │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Page up and down in 10000 element map                                   │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -116 (-25.44%)   │ +2 (0.72%)      │ -1 (-0.36%)  │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -548 (-36.53%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -116 (-25.44%)   │ +2 (0.72%)      │ -1 (-0.36%)  │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -548 (-36.53%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -4_868 (-40.77%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -548 (-36.61%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -548 (-36.61%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -548 (-36.61%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -2_468 (-40.22%) │ +2 (0.30%)      │ -1 (-0.15%)  │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -2_468 (-40.22%) │ +2 (0.30%)      │ -1 (-0.15%)  │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Invert ordering of 10 element map                                       │ -548 (-36.61%)   │ +2 (0.57%)      │ -1 (-0.29%)  │
    │ Invert ordering of 100 element map                                      │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Invert ordering of 101 element map                                      │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Invert ordering of 1000 element map                                     │ -4_916 (-40.79%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    │ Randomly select a row, then change one cell in it.                      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change one cell in it.                      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change one cell in it.                      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Randomly select a row, then change all cells in it.                     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -548 (-36.63%)   │ +2 (0.58%)      │ -1 (-0.29%)  │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -4_868 (-40.78%) │ +2 (0.19%)      │ -1 (-0.09%)  │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴──────────────┘

    ====== Nodes Created (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells        │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .                │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .                │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .                │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .                │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .                │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .                │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .                │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -368 (-29.09%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_008 (-45.30%) │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -368 (-29.09%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_008 (-45.30%) │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_008 (-45.30%) │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -828 (-27.64%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -828 (-27.64%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -828 (-27.64%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -4_508 (-29.13%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -4_508 (-29.13%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -9_108 (-29.31%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .                │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴──────────────┘

    ====== Nodes Recomputed (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬───────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells         │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼───────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ +3 (5.66%)        │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ +4 (7.69%)        │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ +4 (7.69%)        │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ +4 (7.69%)        │ +4 (7.69%)      │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ +4 (7.69%)        │ +4 (7.69%)      │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ +4 (3.28%)        │ +4 (3.28%)      │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ +4 (1.32%)        │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ +4 (1.32%)        │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ +4 (1.32%)        │ +4 (1.32%)      │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ +4 (1.32%)        │ +4 (1.32%)      │ .            │
    │ Focus up and down in 10 element map                                     │ +2 (3.28%)        │ +2 (3.28%)      │ .            │
    │ Focus up and down in 100 element map                                    │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus up and down in 101 element map                                    │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus up and down in 1000 element map                                   │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus up and down in 10000 element map                                  │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 10 rows                              │ +2 (3.28%)        │ +2 (3.28%)      │ .            │
    │ Focus left and right in a map with 100 rows                             │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 101 rows                             │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 1000 rows                            │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Focus left and right in a map with 10000 rows                           │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Page up and down in 10 element map                                      │ +2 (3.28%)        │ +2 (3.28%)      │ .            │
    │ Page up and down in 100 element map                                     │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Page up and down in 101 element map                                     │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Page up and down in 1000 element map                                    │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Page up and down in 10000 element map                                   │ +2 (1.32%)        │ +2 (1.32%)      │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -1_448 (-37.46%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -1_448 (-37.31%)  │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -1_448 (-37.46%)  │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -1_448 (-37.31%)  │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -1_448 (-37.31%)  │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -1_992 (-41.40%)  │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -1_992 (-41.40%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -1_992 (-41.40%)  │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -9_672 (-41.67%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -9_672 (-41.67%)  │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -19_272 (-41.70%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ -284 (-68.43%)    │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ -2_624 (-84.24%)  │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ -2_650 (-84.26%)  │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ -2_650 (-84.26%)  │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                 │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ -1_027 (-47.99%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ -2_215 (-51.51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ -1_027 (-47.99%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ -2_215 (-51.51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ -1_027 (-47.99%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ -2_215 (-51.51%)  │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ -1_027 (-47.99%)  │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ -2_215 (-51.51%)  │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ -6_670 (-53.79%)  │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴───────────────────┴─────────────────┴──────────────┘

    ====== Nodes Invalidated (new - old) ======
    ┌─────────────────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬──────────────┐
    │                                                                         │ dyn cells        │ dyn cols, no cf │ dyn cols, cf │
    ├─────────────────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼──────────────┤
    │ Focus by key (key not present) and unfocus in 10 element map            │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 100 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 101 element map           │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 1000 element map          │ .                │ .               │ .            │
    │ Focus by key (key not present) and unfocus in 10000 element map         │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10 element map                │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 100 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 101 element map               │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 1000 element map              │ .                │ .               │ .            │
    │ Focus by key (key present) and unfocus in 10000 element map             │ .                │ .               │ .            │
    │ Focus up and down in 10 element map                                     │ .                │ .               │ .            │
    │ Focus up and down in 100 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 101 element map                                    │ .                │ .               │ .            │
    │ Focus up and down in 1000 element map                                   │ .                │ .               │ .            │
    │ Focus up and down in 10000 element map                                  │ .                │ .               │ .            │
    │ Focus left and right in a map with 10 rows                              │ .                │ .               │ .            │
    │ Focus left and right in a map with 100 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 101 rows                             │ .                │ .               │ .            │
    │ Focus left and right in a map with 1000 rows                            │ .                │ .               │ .            │
    │ Focus left and right in a map with 10000 rows                           │ .                │ .               │ .            │
    │ Page up and down in 10 element map                                      │ .                │ .               │ .            │
    │ Page up and down in 100 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 101 element map                                     │ .                │ .               │ .            │
    │ Page up and down in 1000 element map                                    │ .                │ .               │ .            │
    │ Page up and down in 10000 element map                                   │ .                │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ -783 (-42.14%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ -530 (-70.20%)   │ .               │ .            │
    │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ -788 (-42.25%)   │ .               │ .            │
    │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ -535 (-70.21%)   │ .               │ .            │
    │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ -85 (-64.39%)    │ .               │ .            │
    │ Apply 4 filters and clear with 100 element map using 10 window          │ -648 (-23.35%)   │ .               │ .            │
    │ Apply 4 filters and clear with 101 element map using 10 window          │ -648 (-23.35%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 10 window         │ -648 (-23.35%)   │ .               │ .            │
    │ Apply 4 filters and clear with 1000 element map using 50 window         │ -3_528 (-24.68%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 50 window        │ -3_528 (-24.68%) │ .               │ .            │
    │ Apply 4 filters and clear with 10000 element map using 100 window       │ -7_128 (-24.84%) │ .               │ .            │
    │ Invert ordering of 10 element map                                       │ .                │ .               │ .            │
    │ Invert ordering of 100 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 101 element map                                      │ .                │ .               │ .            │
    │ Invert ordering of 1000 element map                                     │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change one cell in it.                      │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Randomly select a row, then change all cells in it.                     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ .                │ .               │ .            │
    │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ .                │ .               │ .            │
    │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ .                │ .               │ .            │
    └─────────────────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴──────────────┘
    |}]
;;
