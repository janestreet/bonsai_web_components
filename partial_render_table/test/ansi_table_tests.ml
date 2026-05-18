open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Incr_map_collate
open! Bonsai.Let_syntax
open Shared
module Table = Bonsai_web_partial_render_table
open Bonsai_web_partial_render_table_ansi

module Test = struct
  include Shared.Test

  let create_with_var
    (type a column_id)
    ?(stabilize_height = true)
    ?(visible_range = 0, 100)
    ?(map = Bonsai.Expert.Var.create small_map)
    ?(should_set_bounds = true)
    ~stats
    component
    =
    let min_vis, max_vis = visible_range in
    let filter_var = Bonsai.Expert.Var.create (fun ~key:_ ~data:_ -> true) in
    let { Component.component
        ; get_vdom
        ; get_focus
        ; get_testing
        ; get_inject
        ; get_num_filtered_rows
        ; summarize_focus
        }
      =
      component (Bonsai.Expert.Var.value map) (Bonsai.Expert.Var.value filter_var)
    in
    let handle =
      Handle.create
        (module struct
          type t = a

          let out a = Lazy.force (get_testing a)

          let view a =
            let num_filtered_rows = get_num_filtered_rows a in
            table_to_string
              ~additional_summary:(summarize_focus ?num_filtered_rows (get_focus a))
              (out a)
              ~include_stats:stats
              ()
          ;;

          type incoming = column_id Action.t

          let incoming = get_inject
        end)
        component
    in
    let t = { handle; get_vdom; input_var = map; filter_var; get_num_filtered_rows } in
    if should_set_bounds then set_bounds t ~low:min_vis ~high:max_vis;
    (* Because the component uses edge-triggering to propagate rank-range, we need to run
       the view-computers twice. *)
    if stabilize_height
    then (
      Handle.store_view handle;
      Handle.store_view handle);
    t
  ;;

  let create
    ?stabilize_height
    ?visible_range
    ?(map = small_map)
    ?should_set_bounds
    ~stats
    component
    =
    create_with_var
      ?stabilize_height
      ?visible_range
      ~map:(Bonsai.Expert.Var.create map)
      ?should_set_bounds
      ~stats
      component
  ;;
end

let%expect_test "basic table" =
  let test = Test.create ~stats:true (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "basic table with cell focus" =
  let test = Test.create ~stats:true (Test.Component.default_cell_focus ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "basic table with default sort" =
  let test =
    Test.create
      ~stats:true
      (Test.Component.default
         ~default_sort:
           (Bonsai.return (fun (_key, { a = a1; _ }) (_key, { a = a2; _ }) ->
              -String.compare a1 a2))
         ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 4   │ world │ 2.000000 │ --- │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 0   │ hello │ 1.000000 │ 1   │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "basic table with overriden default sort" =
  let override_sort_var = Bonsai.Expert.Var.create (fun _k c -> c) in
  let override_sort = Bonsai.Expert.Var.value override_sort_var in
  let test =
    Test.create
      ~stats:true
      (Test.Component.default
         ~override_sort
         ~default_sort:
           (Bonsai.return (fun (_key, { a = a1; _ }) (_key, { a = a2; _ }) ->
              -String.compare a1 a2))
         ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 4   │ world │ 2.000000 │ --- │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 0   │ hello │ 1.000000 │ 1   │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set override_sort_var (fun _k c -> Comparable.reverse c);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "basic table with overriden column sort" =
  let override_sort_var = Bonsai.Expert.Var.create (fun _k c -> c) in
  let override_sort = Bonsai.Expert.Var.value override_sort_var in
  let test =
    Test.create
      ~stats:true
      (Test.Component.default
         ~override_sort
         ~default_sort:
           (Bonsai.return (fun (_key, { a = a1; _ }) (_key, { a = a2; _ }) ->
              -String.compare a1 a2))
         ())
  in
  Handle.click_on test.handle ~selector:"td:nth-child(1) > div" ~get_vdom:test.get_vdom;
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key ▲ │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set override_sort_var (fun _k c -> Comparable.reverse c);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 3     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key ▲ │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ 4     │ world │ 2.000000 │ --- │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 0     │ hello │ 1.000000 │ 1   │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "REGRESSION: basic table with overriden column sort but no default sort \
                 applies the override to the wrong comparator"
  =
  let override_sort_var =
    Bonsai.Expert.Var.create (fun by_key c ->
      Comparable.lexicographic [ c; Comparable.lift by_key ~f:Tuple2.get1 ])
  in
  let override_sort = Bonsai.Expert.Var.value override_sort_var in
  let test = Test.create ~stats:false (Test.Component.default ~override_sort ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  let override_called = ref false in
  Bonsai.Expert.Var.set override_sort_var (fun by_key c a b ->
    override_called := true;
    Comparable.reverse
      (Comparable.lexicographic [ c; Comparable.lift by_key ~f:Tuple2.get1 ])
      a
      b);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 4   │ world │ 2.000000 │ --- │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 0   │ hello │ 1.000000 │ 1   │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  (* We can observe that the override is, in fact, being called. *)
  print_s [%message (!override_called : bool)];
  [%expect {| (!override_called true) |}];
  ()
;;

let%expect_test "BUG: In basic tables with dynamic columns, the sorted column can change \
                 if the order of columns changes"
  =
  let module Record = struct
    module T = struct
      type t =
        { a : int
        ; b : int
        }
      [@@deriving compare, equal, sexp]
    end

    include T
    include Comparator.Make (T)
  end
  in
  let map =
    [ 0; 1; 2 ]
    |> List.map ~f:(fun i -> i, { Record.a = i; b = (i + 1) % 3 })
    |> Int.Map.of_alist_exn
    |> Bonsai.return
  in
  let a_before_b = Bonsai.Expert.Var.create false in
  let columns =
    let int_column name get_data =
      Table.Basic.Columns.Dynamic_columns.column
        ~header:(Vdom.Node.text name)
        ~sort:(fun (_, a) (_, b) -> Int.ascending (get_data a) (get_data b))
        ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Int.to_string (get_data data)))
        ()
    in
    let column_a = int_column "a" (fun (data : Record.t) -> data.a) in
    let column_b = int_column "b" (fun (data : Record.t) -> data.b) in
    let ordered =
      let%map a_before_b = Bonsai.Expert.Var.value a_before_b in
      if a_before_b then [ column_a; column_b ] else [ column_b; column_a ]
    in
    Table.Basic.Columns.Dynamic_columns.lift ordered
  in
  let component =
    Table.Basic.component
      ~focus:None
      ~row_height:(Bonsai.return (`Px 20))
      ~columns
      (module Int)
      map
  in
  let module Indexed_column_id = Table.Basic.Columns.Indexed_column_id in
  let handle =
    Handle.create
      (module struct
        type t = (unit, int, Indexed_column_id.t) Table.Basic.Result.t
        type incoming = unit

        let view { Table.Basic.Result.for_testing; _ } =
          table_to_string (Lazy.force for_testing) ~include_stats:false ()
        ;;

        let incoming { Table.Basic.Result.sortable_state; _ } () =
          Table.Basic.Columns.Dynamic_columns.Sortable.inject
            sortable_state
            (* Sort ascending on the first column *)
            (Set_sort (Indexed_column_id.of_int 0, Asc_to_desc_to_none))
        ;;
      end)
      component
  in
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    ┌───┬─────┬───┬───┐
    │ > │ #   │ b │ a │
    ├───┼─────┼───┼───┤
    │   │ 0   │ 1 │ 0 │
    │   │ 100 │ 2 │ 1 │
    │   │ 200 │ 0 │ 2 │
    └───┴─────┴───┴───┘
    |}];
  Handle.do_actions handle [ () ];
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    ┌───┬─────┬─────┬───┐
    │ > │ #   │ b ▲ │ a │
    ├───┼─────┼─────┼───┤
    │   │ 0   │ 0   │ 2 │
    │   │ 100 │ 1   │ 0 │
    │   │ 200 │ 2   │ 1 │
    └───┴─────┴─────┴───┘
    |}];
  Bonsai.Expert.Var.set a_before_b true;
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  (* Notice that the order of the columns switched, but so has the sort order: column a is
     now sorted ascending, and column b is no longer. This is a bug. *)
  [%expect
    {|
    ┌───┬─────┬─────┬───┐
    │ > │ #   │ a ▲ │ b │
    ├───┼─────┼─────┼───┤
    │   │ 0   │ 0   │ 1 │
    │   │ 100 │ 1   │ 2 │
    │   │ 200 │ 2   │ 0 │
    └───┴─────┴─────┴───┘
    |}]
;;

let%expect_test "big table" =
  let test =
    Test.create
      ~stats:true
      ~map:big_map
      ~visible_range:(0, 10)
      (Test.Component.default ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 87    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 900  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 1000 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 1100 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "table with some preload" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:true
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 2     │
    │ rows-after     │ 85    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "big table filtered" =
  let test =
    Test.create
      ~stats:true
      ~map:big_map
      ~visible_range:(0, 10)
      (Test.Component.default ())
  in
  Bonsai.Expert.Var.set test.filter_var (fun ~key ~data:_ -> key mod 2 = 0);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (49)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 37    │
    │ num-filtered   │ 49    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 2   │ hi │ 1.000000  │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000  │ 100 │
    │   │ 200  │ 6   │ hi │ 3.000000  │ 100 │
    │   │ 300  │ 8   │ hi │ 4.000000  │ 100 │
    │   │ 400  │ 10  │ hi │ 5.000000  │ 100 │
    │   │ 500  │ 12  │ hi │ 6.000000  │ 100 │
    │   │ 600  │ 14  │ hi │ 7.000000  │ 100 │
    │   │ 700  │ 16  │ hi │ 8.000000  │ 100 │
    │   │ 800  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 900  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 1000 │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 1100 │ 24  │ hi │ 12.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}]
;;

let%expect_test "table with col groups" =
  let test =
    Test.create
      ~stats:true
      ~map:groups_map
      ~visible_range:(0, 10)
      (Test.Component.default' ~with_groups:true ())
  in
  Bonsai.Expert.Var.set test.filter_var (fun ~key ~data:_ -> key mod 2 = 0);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (2)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 0     │
    │ num-filtered   │ 2     │
    │ num-unfiltered │ 3     │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬────────┬──────────┬─────────┬─────────┬─────────┐
    │ > │ #   │ key │ Basics │ Basics   │ Level 1 │ Level 1 │ Level 1 │
    │   │     │     │ a      │ b        │ Level 2 │ Level 2 │ e       │
    │   │     │     │        │          │ c       │ d       │         │
    ├───┼─────┼─────┼────────┼──────────┼─────────┼─────────┼─────────┤
    │   │ 0   │ 0   │ hello  │ 1.000000 │ apple   │ 100     │ 1st     │
    │   │ 100 │ 4   │ world  │ 2.000000 │ pear    │ ---     │ 2nd     │
    └───┴─────┴─────┴────────┴──────────┴─────────┴─────────┴─────────┘
    |}]
;;

let%expect_test "locking focus prevents moving focus" =
  let test = Test.create ~stats:false (Test.Component.default_cell_focus ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((0 0)))
    ((focused ((0 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ > 0 < │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Lock_focus ];
  Handle.show_diff test.handle;
  [%expect {| |}];
  (* No interactions cause diffs while focus is locked *)
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.do_actions test.handle [ Focus_left ];
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.do_actions test.handle [ Focus_right ];
  [%expect {| |}];
  Handle.do_actions test.handle [ Unlock_focus ];
  Handle.show_diff test.handle;
  [%expect {| |}];
  (* Now that the table is unlocked, we can move the focus again *)
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show_diff test.handle;
  [%expect
    {|
    scrolling cell at row index 1 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))

    -|((focused ((0 0))) (num_filtered_rows (3)))
    +|((focused ((1 0))) (num_filtered_rows (3)))
      ┌───┬─────┬───────┬───────┬──────────┬─────┐
      │ > │ #   │ key   │ a     │ b        │ d   │
      ├───┼─────┼───────┼───────┼──────────┼─────┤
    -|│   │ 0   │ > 0 < │ hello │ 1.000000 │ 1   │
    +|│   │ 0   │ 0     │ hello │ 1.000000 │ 1   │
    -|│   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    +|│   │ 100 │ > 1 < │ there │ 2.000000 │ 2   │
      │   │ 200 │ 4     │ world │ 2.000000 │ --- │
      └───┴─────┴───────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus down in row-focus table" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    ((focused (0)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │ * │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus down in cell-focus table" =
  let test = Test.create ~stats:false (Test.Component.default_cell_focus ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((0 0)))
    ((focused ((0 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ > 0 < │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 1 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │ 1   │
    │   │ 100 │ > 1 < │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus up in row-focus table" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │ * │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus up in cell-focus table" =
  let test = Test.create ~stats:false (Test.Component.default_cell_focus ()) in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 2 and column id 0 into view, if necessary
    (focus_changed_to ((4 0)))
    ((focused ((4 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ > 4 < │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 1 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ 0     │ hello │ 1.000000 │ 1   │
    │   │ 100 │ > 1 < │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "unfocus" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │ * │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "remove focused moves down if possible" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (2)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus shadow (down)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    ((focused (0)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │ * │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │ * │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus shadow (up)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │ * │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    ((focused (0)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │ * │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "focus shadow (right)" =
  let test = Test.create ~stats:false (Test.Component.default_cell_focus ()) in
  Handle.do_actions test.handle [ Focus_right ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((0 0)))
    ((focused ((0 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ > 0 < │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_right ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 1 into view, if necessary
    (focus_changed_to ((0 1)))
    ((focused ((0 1))) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────────┬──────────┬─────┐
    │ > │ #   │ key │ a         │ b        │ d   │
    ├───┼─────┼─────┼───────────┼──────────┼─────┤
    │   │ 0   │ 0   │ > hello < │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there     │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world     │ 2.000000 │ --- │
    └───┴─────┴─────┴───────────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_right ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 2 into view, if necessary
    (focus_changed_to ((0 2)))
    ((focused ((0 2))) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────────┬─────┐
    │ > │ #   │ key │ a     │ b            │ d   │
    ├───┼─────┼─────┼───────┼──────────────┼─────┤
    │   │ 0   │ 0   │ hello │ > 1.000000 < │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000     │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000     │ --- │
    └───┴─────┴─────┴───────┴──────────────┴─────┘
    |}]
;;

let%expect_test "focus shadow (left)" =
  let test = Test.create ~stats:false (Test.Component.default_cell_focus ()) in
  Handle.do_actions test.handle [ Focus_cell (0, 2) ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 2 into view, if necessary
    (focus_changed_to ((0 2)))
    ((focused ((0 2))) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────────┬─────┐
    │ > │ #   │ key │ a     │ b            │ d   │
    ├───┼─────┼─────┼───────┼──────────────┼─────┤
    │   │ 0   │ 0   │ hello │ > 1.000000 < │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000     │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000     │ --- │
    └───┴─────┴─────┴───────┴──────────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_left ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 1 into view, if necessary
    (focus_changed_to ((0 1)))
    ((focused ((0 1))) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────────┬──────────┬─────┐
    │ > │ #   │ key │ a         │ b        │ d   │
    ├───┼─────┼─────┼───────────┼──────────┼─────┤
    │   │ 0   │ 0   │ > hello < │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there     │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world     │ 2.000000 │ --- │
    └───┴─────┴─────┴───────────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_left ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((0 0)))
    ((focused ((0 0))) (num_filtered_rows (3)))
    ┌───┬─────┬───────┬───────┬──────────┬─────┐
    │ > │ #   │ key   │ a     │ b        │ d   │
    ├───┼─────┼───────┼───────┼──────────┼─────┤
    │   │ 0   │ > 0 < │ hello │ 1.000000 │ 1   │
    │   │ 100 │ 1     │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4     │ world │ 2.000000 │ --- │
    └───┴─────┴───────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "remove focused causes unfocus (down)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (2)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (2)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "remove focused causes unfocus (up)" =
  let test = Test.create ~stats:false (Test.Component.default ()) in
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    skipping scroll because target already in view
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │ * │ 100 │ 1   │ there │ 2.000000 │ 2   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update test.input_var ~f:(fun map -> Map.remove map 1);
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (2)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │   │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (0))
    ((focused (0)) (num_filtered_rows (2)))
    ┌───┬─────┬─────┬───────┬──────────┬─────┐
    │ > │ #   │ key │ a     │ b        │ d   │
    ├───┼─────┼─────┼───────┼──────────┼─────┤
    │ * │ 0   │ 0   │ hello │ 1.000000 │ 1   │
    │   │ 200 │ 4   │ world │ 2.000000 │ --- │
    └───┴─────┴─────┴───────┴──────────┴─────┘
    |}]
;;

let%expect_test "select index" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index 3 ];
  Handle.show test.handle;
  (* Jump to a normal row in view. *)
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (4))
    ((focused (4)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │ * │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index 0 ];
  Handle.show test.handle;
  (* Jump to the first row. *)
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index (-10) ];
  Handle.show test.handle;
  (* Jump to a negative row (should be handled robustly by the table) *)
  [%expect
    {|
    scrolling to index 0 at 0.0px
    ((focused (1)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index 30 ];
  Handle.show test.handle;
  (* Jump to a row out of view. *)
  [%expect
    {|
    scrolling to index 30 at 31.0px
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:30 ~high:33;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* Changing the visible range brings the focused item into view, but does not
     immediately fire the focus-changed callback. *)
  [%expect
    {|
    ((focused (31)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 29  │ hi │ 14.000000 │ 100 │
    │   │ 100 │ 30  │ hi │ 15.000000 │ 100 │
    │ * │ 200 │ 31  │ hi │ 15.000000 │ 100 │
    │   │ 300 │ 32  │ hi │ 16.000000 │ 100 │
    │   │ 400 │ 33  │ hi │ 16.000000 │ 100 │
    │   │ 500 │ 34  │ hi │ 17.000000 │ 100 │
    │   │ 600 │ 35  │ hi │ 17.000000 │ 100 │
    │   │ 700 │ 36  │ hi │ 18.000000 │ 100 │
    │   │ 800 │ 37  │ hi │ 18.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.show test.handle;
  (* A frame later we notice that the focus changed and fire the callback. *)
  [%expect
    {|
    (focus_changed_to (31))
    ((focused (31)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 29  │ hi │ 14.000000 │ 100 │
    │   │ 100 │ 30  │ hi │ 15.000000 │ 100 │
    │ * │ 200 │ 31  │ hi │ 15.000000 │ 100 │
    │   │ 300 │ 32  │ hi │ 16.000000 │ 100 │
    │   │ 400 │ 33  │ hi │ 16.000000 │ 100 │
    │   │ 500 │ 34  │ hi │ 17.000000 │ 100 │
    │   │ 600 │ 35  │ hi │ 17.000000 │ 100 │
    │   │ 700 │ 36  │ hi │ 18.000000 │ 100 │
    │   │ 800 │ 37  │ hi │ 18.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Test.set_bounds test ~low:95 ~high:110;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* Go to the last part of the table. *)
  [%expect
    {|
    ((focused (31)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │   │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index 99 ];
  Handle.show test.handle;
  (* Focus the last row in the table. *)
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (99))
    ((focused (99)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │ * │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_index 110 ];
  Handle.show test.handle;
  (* Focus an index off the end of the table (should be handled robustly by the table). *)
  [%expect
    {|
    skipping scroll because target already in view
    ((focused (99)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │ * │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}]
;;

let%expect_test "page up" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 5 at 5.0px
    (focus_changed_to (6))
    ((focused (6)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │ * │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "page down and page up" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  (* Page down starts the focus at the bottom of the visible range. *)
  [%expect
    {|
    scrolling to index 11 at 12.0px
    (focus_changed_to (12))
    ((focused (12)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │ * │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  (* Page down at the bottom of the page moves focus off the page to an item that isn't
     yet in the collated map. You can't actually tell this is happening right here, but
     later in the test when the focused item becomes visible, you can infer that this is
     what happened. *)
  [%expect
    {|
    scrolling to index 17 at 18.0px
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  (* Page down while the focused item is at an index instead of an existing row. The focus
     should be moved further down into the table. Again, you'll only see evidence of this
     later in the test when the focused row becomes visible. *)
  [%expect
    {|
    scrolling to index 23 at 24.0px
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:18 ~high:23;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* The focused index is brought into view, demonstrating that the PRT has properly kept
     track of focus even when the focused index didn't have a corresponding row. *)
  [%expect
    {|
    ((focused (24)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │   │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │ * │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.show test.handle;
  (* The next frame, the table notices that the the focused row is now visible, so it
     fires the focus changed event to notify users of the table. *)
  [%expect
    {|
    (focus_changed_to (24))
    ((focused (24)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │   │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │ * │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  (* We move the focus upward so that the next page up doesn't jump far. *)
  Handle.do_actions test.handle [ Focus_up; Focus_up; Focus_up; Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (23))
    skipping scroll because target already in view
    (focus_changed_to (22))
    skipping scroll because target already in view
    (focus_changed_to (21))
    skipping scroll because target already in view
    (focus_changed_to (20))
    ((focused (20)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │ * │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │   │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  (* Page up jumps to the top of the page if it isn't already there. In this case, it only
     jumped one row, which demonstrates that page up is showing some restraint. *)
  [%expect
    {|
    scrolling to index 18 at 18.0px
    (focus_changed_to (19))
    ((focused (19)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │ * │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │   │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │   │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  (* A second page up brings the focus out of view. *)
  [%expect
    {|
    scrolling to index 12 at 12.0px
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │   │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │   │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  (* Page up brings the focus even further out of view. *)
  [%expect
    {|
    scrolling to index 6 at 6.0px
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 17  │ hi │ 8.000000  │ 100 │
    │   │ 100  │ 18  │ hi │ 9.000000  │ 100 │
    │   │ 200  │ 19  │ hi │ 9.000000  │ 100 │
    │   │ 300  │ 20  │ hi │ 10.000000 │ 100 │
    │   │ 400  │ 21  │ hi │ 10.000000 │ 100 │
    │   │ 500  │ 22  │ hi │ 11.000000 │ 100 │
    │   │ 600  │ 23  │ hi │ 11.000000 │ 100 │
    │   │ 700  │ 24  │ hi │ 12.000000 │ 100 │
    │   │ 800  │ 25  │ hi │ 12.000000 │ 100 │
    │   │ 900  │ 26  │ hi │ 13.000000 │ 100 │
    │   │ 1000 │ 27  │ hi │ 13.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Test.set_bounds test ~low:5 ~high:10;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* Bringing the viewport up to where focus is shows that it has progressed as far as we
     expected. *)
  [%expect
    {|
    ((focused (7)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │ * │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.show test.handle;
  (* One frame later, we also notice that the focused row exists, so we can trigger the
     focus changed callback. *)
  [%expect
    {|
    (focus_changed_to (7))
    ((focused (7)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 100  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 200  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 300  │ 6   │ hi │ 3.000000 │ 100 │
    │ * │ 400  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 500  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 600  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 700  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 800  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 900  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1000 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1100 │ 14  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "page up stops at the top of the table (when it is in view)" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  (* The first page up puts focus at the top of the table. *)
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up ];
  Handle.show test.handle;
  (* A subsequent page up does nothing, since the focus is as high as it possible can be. *)
  [%expect
    {|
    scrolling to index 0 at 0.0px
    ((focused (1)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800 │ 9   │ hi │ 4.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "page up stops at the top of the table (when it is out of view)" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(10, 15)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 100  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 200  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 300  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 400  │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 500  │ 14  │ hi │ 7.000000 │ 100 │
    │   │ 600  │ 15  │ hi │ 7.000000 │ 100 │
    │   │ 700  │ 16  │ hi │ 8.000000 │ 100 │
    │   │ 800  │ 17  │ hi │ 8.000000 │ 100 │
    │   │ 900  │ 18  │ hi │ 9.000000 │ 100 │
    │   │ 1000 │ 19  │ hi │ 9.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_up; Page_up; Page_up; Page_up; Page_up ];
  Handle.show test.handle;
  (* A few page ups should put the focus at the top of the table and stop there. *)
  [%expect
    {|
    scrolling to index 10 at 10.0px
    (focus_changed_to (11))
    scrolling to index 4 at 4.0px
    (focus_changed_to ())
    scrolling to index 0 at 0.0px
    scrolling to index 0 at 0.0px
    scrolling to index 0 at 0.0px
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 0    │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 100  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 200  │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 300  │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 400  │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 500  │ 14  │ hi │ 7.000000 │ 100 │
    │   │ 600  │ 15  │ hi │ 7.000000 │ 100 │
    │   │ 700  │ 16  │ hi │ 8.000000 │ 100 │
    │   │ 800  │ 17  │ hi │ 8.000000 │ 100 │
    │   │ 900  │ 18  │ hi │ 9.000000 │ 100 │
    │   │ 1000 │ 19  │ hi │ 9.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:0 ~high:5;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* A subsequent page up does nothing, since the focus is as high as it possible can be. *)
  [%expect
    {|
    ((focused (1)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │ * │ -800 │ 1   │ hi │ 0.000000 │ 100 │
    │   │ -700 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ -600 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ -500 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ -400 │ 5   │ hi │ 2.000000 │ 100 │
    │   │ -300 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ -200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ -100 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 0    │ 9   │ hi │ 4.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "page down stops at the bottom of the table (when it is in view)" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(95, 110)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │   │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  (* The first page down puts focus at the bottom of the table. *)
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (99))
    ((focused (99)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │ * │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down ];
  Handle.show test.handle;
  (* A subsequent page down does nothing, since the focus is as low as it possible can be. *)
  [%expect
    {|
    skipping scroll because target already in view
    ((focused (99)) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬───────────┬─────┐
    │ > │ #   │ key │ a  │ b         │ d   │
    ├───┼─────┼─────┼────┼───────────┼─────┤
    │   │ 0   │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 500 │ 98  │ hi │ 49.000000 │ 100 │
    │ * │ 600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴─────┴─────┴────┴───────────┴─────┘
    |}]
;;

let%expect_test "page down stops at the bottom of the table (when it is out of view)" =
  let test =
    Test.create
      ~map:big_map
      ~visible_range:(85, 90)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 83  │ hi │ 41.000000 │ 100 │
    │   │ 100  │ 84  │ hi │ 42.000000 │ 100 │
    │   │ 200  │ 85  │ hi │ 42.000000 │ 100 │
    │   │ 300  │ 86  │ hi │ 43.000000 │ 100 │
    │   │ 400  │ 87  │ hi │ 43.000000 │ 100 │
    │   │ 500  │ 88  │ hi │ 44.000000 │ 100 │
    │   │ 600  │ 89  │ hi │ 44.000000 │ 100 │
    │   │ 700  │ 90  │ hi │ 45.000000 │ 100 │
    │   │ 800  │ 91  │ hi │ 45.000000 │ 100 │
    │   │ 900  │ 92  │ hi │ 46.000000 │ 100 │
    │   │ 1000 │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 1100 │ 94  │ hi │ 47.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Page_down; Page_down; Page_down; Page_down; Page_down ];
  Handle.show test.handle;
  (* A few page downs should put the focus at the bottom of the table and stop there. *)
  [%expect
    {|
    scrolling to index 91 at 92.0px
    (focus_changed_to (92))
    scrolling to index 97 at 98.0px
    (focus_changed_to ())
    scrolling to index 98 at 99.0px
    scrolling to index 98 at 99.0px
    scrolling to index 98 at 99.0px
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 0    │ 83  │ hi │ 41.000000 │ 100 │
    │   │ 100  │ 84  │ hi │ 42.000000 │ 100 │
    │   │ 200  │ 85  │ hi │ 42.000000 │ 100 │
    │   │ 300  │ 86  │ hi │ 43.000000 │ 100 │
    │   │ 400  │ 87  │ hi │ 43.000000 │ 100 │
    │   │ 500  │ 88  │ hi │ 44.000000 │ 100 │
    │   │ 600  │ 89  │ hi │ 44.000000 │ 100 │
    │   │ 700  │ 90  │ hi │ 45.000000 │ 100 │
    │   │ 800  │ 91  │ hi │ 45.000000 │ 100 │
    │   │ 900  │ 92  │ hi │ 46.000000 │ 100 │
    │   │ 1000 │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 1100 │ 94  │ hi │ 47.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}];
  Test.set_bounds test ~low:95 ~high:110;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  (* A subsequent page down does nothing, since the focus is as low as it possible can be. *)
  [%expect
    {|
    ((focused (99)) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬───────────┬─────┐
    │ > │ #    │ key │ a  │ b         │ d   │
    ├───┼──────┼─────┼────┼───────────┼─────┤
    │   │ 1000 │ 93  │ hi │ 46.000000 │ 100 │
    │   │ 1100 │ 94  │ hi │ 47.000000 │ 100 │
    │   │ 1200 │ 95  │ hi │ 47.000000 │ 100 │
    │   │ 1300 │ 96  │ hi │ 48.000000 │ 100 │
    │   │ 1400 │ 97  │ hi │ 48.000000 │ 100 │
    │   │ 1500 │ 98  │ hi │ 49.000000 │ 100 │
    │ * │ 1600 │ 99  │ hi │ 49.000000 │ 100 │
    └───┴──────┴─────┴────┴───────────┴─────┘
    |}]
;;

let%expect_test "actions on empty table" =
  let test =
    Test.create
      ~map:(Map.empty (module Int))
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  (* just make sure nothing weird happens *)
  Handle.do_actions test.handle [ Page_down; Page_up; Focus_down; Focus_up; Unfocus ];
  [%expect {| |}]
;;

let%expect_test "moving focus down should work even when the index changes" =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    skipping scroll because target already in view
    (focus_changed_to (2))
    ((focused (2)) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │ * │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update map ~f:(fun map -> Map.remove map 1);
  Handle.show test.handle;
  [%expect
    {|
    ((focused (2)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (3))
    ((focused (3)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "moving focus down should work even when the index changes and focus is \
                 shadow"
  =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    skipping scroll because target already in view
    (focus_changed_to (2))
    ((focused (2)) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │ * │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update map ~f:(fun map -> Map.remove map 1);
  Handle.show test.handle;
  [%expect
    {|
    ((focused (2)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │ * │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (3))
    ((focused (3)) (num_filtered_rows (3)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "moving focus up should work even when the index changes" =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down; Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    skipping scroll because target already in view
    (focus_changed_to (2))
    skipping scroll because target already in view
    (focus_changed_to (3))
    ((focused (3)) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update map ~f:(fun map ->
    Map.add_exn map ~key:0 ~data:{ a = "hi"; b = 0.0; c = "c"; d = Some 100; e = "e" });
  Handle.show test.handle;
  [%expect
    {|
    ((focused (3)) (num_filtered_rows (5)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -100 │ 0   │ hi │ 0.000000 │ 100 │
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (2))
    ((focused (2)) (num_filtered_rows (5)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -100 │ 0   │ hi │ 0.000000 │ 100 │
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │ * │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "moving focus up should work even when the index changes and focus is \
                 shadow"
  =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down; Focus_down; Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    skipping scroll because target already in view
    (focus_changed_to (2))
    skipping scroll because target already in view
    (focus_changed_to (3))
    ((focused (3)) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.update map ~f:(fun map ->
    Map.add_exn map ~key:0 ~data:{ a = "hi"; b = 0.0; c = "c"; d = Some 100; e = "e" });
  Handle.show test.handle;
  [%expect
    {|
    ((focused (3)) (num_filtered_rows (5)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -100 │ 0   │ hi │ 0.000000 │ 100 │
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │ * │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (5)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -100 │ 0   │ hi │ 0.000000 │ 100 │
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_up ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (2))
    ((focused (2)) (num_filtered_rows (5)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -100 │ 0   │ hi │ 0.000000 │ 100 │
    │   │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │ * │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "directional jumping" =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default_cell_focus ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_top ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ > 1 < │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4     │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_bottom ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 0 into view, if necessary
    (focus_changed_to ((4 0)))
    ((focused ((4 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ 1     │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ > 4 < │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_rightmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 3 into view, if necessary
    (focus_changed_to ((4 3)))
    ((focused ((4 3))) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────────┐
    │ > │ #   │ key │ a  │ b        │ d       │
    ├───┼─────┼─────┼────┼──────────┼─────────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100     │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100     │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100     │
    │   │ 300 │ 4   │ hi │ 2.000000 │ > 100 < │
    └───┴─────┴─────┴────┴──────────┴─────────┘
    |}];
  Handle.do_actions test.handle [ Focus_leftmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 0 into view, if necessary
    (focus_changed_to ((4 0)))
    ((focused ((4 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ 1     │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ > 4 < │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  (* We repeat this one, because the first time we ran it, we didn't start with a focused
     state. *)
  Handle.do_actions test.handle [ Focus_top ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ > 1 < │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4     │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "directional jumping in row-focus table" =
  let map =
    List.range 1 20
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(5, 10)
      ~stats:false
      (Test.Component.default ())
  in
  Handle.do_actions test.handle [ Focus_index 8 ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    (focus_changed_to (9))
    ((focused (9)) (num_filtered_rows (19)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │ * │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_top ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (19)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:0 ~high:5;
  Handle.recompute_view test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused (1)) (num_filtered_rows (19)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │ * │ -400 │ 1   │ hi │ 0.000000 │ 100 │
    │   │ -300 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ -200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ -100 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 0    │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200  │ 7   │ hi │ 3.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:15 ~high:20;
  Handle.recompute_view test.handle;
  Handle.do_actions test.handle [ Focus_bottom ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to (1))
    skipping scroll because target already in view
    (focus_changed_to (19))
    ((focused (19)) (num_filtered_rows (19)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 15  │ hi │ 7.000000 │ 100 │
    │   │ 100 │ 16  │ hi │ 8.000000 │ 100 │
    │   │ 200 │ 17  │ hi │ 8.000000 │ 100 │
    │   │ 300 │ 18  │ hi │ 9.000000 │ 100 │
    │ * │ 400 │ 19  │ hi │ 9.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "directional jumping from unfocused" =
  let map =
    [ 1; 2; 3; 4 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int (i / 2); c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.Expert.Var.create
  in
  let test =
    Test.create_with_var
      ~map
      ~visible_range:(0, 5)
      ~stats:false
      (Test.Component.default_cell_focus ~preload_rows:2 ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_top ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ > 1 < │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4     │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_rightmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 3 into view, if necessary
    (focus_changed_to ((1 3)))
    ((focused ((1 3))) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────────┐
    │ > │ #   │ key │ a  │ b        │ d       │
    ├───┼─────┼─────┼────┼──────────┼─────────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ > 100 < │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100     │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100     │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100     │
    └───┴─────┴─────┴────┴──────────┴─────────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_leftmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 0 and column id 0 into view, if necessary
    (focus_changed_to ((1 0)))
    ((focused ((1 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ > 1 < │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4     │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_bottom ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 0 into view, if necessary
    (focus_changed_to ((4 0)))
    ((focused ((4 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ 1     │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ > 4 < │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  (* Focus is at the bottom, not the top, because we still keep around "shadow" focus. *)
  Handle.do_actions test.handle [ Focus_rightmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 3 into view, if necessary
    (focus_changed_to ((4 3)))
    ((focused ((4 3))) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────────┐
    │ > │ #   │ key │ a  │ b        │ d       │
    ├───┼─────┼─────┼────┼──────────┼─────────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100     │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100     │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100     │
    │   │ 300 │ 4   │ hi │ 2.000000 │ > 100 < │
    └───┴─────┴─────┴────┴──────────┴─────────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_leftmost ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 0 into view, if necessary
    (focus_changed_to ((4 0)))
    ((focused ((4 0))) (num_filtered_rows (4)))
    ┌───┬─────┬───────┬────┬──────────┬─────┐
    │ > │ #   │ key   │ a  │ b        │ d   │
    ├───┼─────┼───────┼────┼──────────┼─────┤
    │   │ 0   │ 1     │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2     │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3     │ hi │ 1.000000 │ 100 │
    │   │ 300 │ > 4 < │ hi │ 2.000000 │ 100 │
    └───┴─────┴───────┴────┴──────────┴─────┘
    |}];
  (* Focus is at the right, not the left, because we still keep around "shadow" focus. *)
  Handle.do_actions test.handle [ Focus_rightmost ];
  Handle.do_actions test.handle [ Focus_top ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 3 into view, if necessary
    (focus_changed_to ((4 3)))
    scrolling cell at row index 0 and column id 3 into view, if necessary
    (focus_changed_to ((1 3)))
    ((focused ((1 3))) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────────┐
    │ > │ #   │ key │ a  │ b        │ d       │
    ├───┼─────┼─────┼────┼──────────┼─────────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ > 100 < │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100     │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100     │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100     │
    └───┴─────┴─────┴────┴──────────┴─────────┘
    |}];
  Handle.do_actions test.handle [ Unfocus ];
  Handle.show test.handle;
  [%expect
    {|
    (focus_changed_to ())
    ((focused ()) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300 │ 4   │ hi │ 2.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_bottom ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling cell at row index 3 and column id 3 into view, if necessary
    (focus_changed_to ((4 3)))
    ((focused ((4 3))) (num_filtered_rows (4)))
    ┌───┬─────┬─────┬────┬──────────┬─────────┐
    │ > │ #   │ key │ a  │ b        │ d       │
    ├───┼─────┼─────┼────┼──────────┼─────────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100     │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100     │
    │   │ 200 │ 3   │ hi │ 1.000000 │ 100     │
    │   │ 300 │ 4   │ hi │ 2.000000 │ > 100 < │
    └───┴─────┴─────┴────┴──────────┴─────────┘
    |}]
;;

let%expect_test "Pseudo-BUG: setting rank_range does not change the which rows the \
                 for_testing output will display"
  =
  let module Action = struct
    type t = Focus_down
  end
  in
  let rank_range = Bonsai.Expert.Var.create (Collate_params.Which_range.To 2) in
  let map =
    [ 1; 2; 3; 4; 5; 6; 7 ]
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int i; c = "c"; d = Some 100; e = "e" })
    |> Int.Map.of_alist_exn
    |> Bonsai.return
  in
  let component (local_ graph) =
    let collate, key_rank =
      let collate =
        let%map rank_range = Bonsai.Expert.Var.value rank_range in
        { Collate_params.Stable.V1.filter = None
        ; order = Compare.Unchanged
        ; key_range = Collate_params.Which_range.All_rows
        ; rank_range
        }
        |> Collate_params.of_stable_v1
      in
      Table_expert.collate
        ~filter_equal:phys_equal
        ~filter_to_predicate:Fn.id
        ~order_equal:phys_equal
        ~order_to_compare:Fn.id
        map
        collate
        graph
    in
    Table_expert.component
      (module Int)
      ~focus:
        (Table_expert.Focus.By_row
           { on_change = Test.focus_changed
           ; compute_presence =
               (fun focus (local_ _graph) ->
                 let%arr map and focus in
                 match focus with
                 | None -> None
                 | Some focus -> if Map.mem map focus then Some focus else None)
           ; key_rank
           })
      ~row_height:(Bonsai.return (`Px 20))
      ~columns:
        (Bonsai.return
         @@ [ Table_expert.Columns.Dynamic_columns.column
                ~header:(Vdom.Node.text "a")
                ~cell:(fun ~key:_ ~data -> Vdom.Node.text data.a)
                ()
            ; Table_expert.Columns.Dynamic_columns.column
                ~header:(Vdom.Node.text "b")
                ~cell:(fun ~key:_ ~data -> Vdom.Node.text (Float.to_string data.b))
                ()
            ]
         |> Table_expert.Columns.Dynamic_columns.lift)
      collate
      graph
  in
  let handle =
    Handle.create
      (module struct
        type t =
          ( int Table_expert.Focus.By_row.optional
            , Indexed_column_id.t )
            Table_expert.Result.t

        type incoming = Action.t

        let view { Table_expert.Result.for_testing; focus; _ } =
          let for_testing = Lazy.force for_testing in
          let focused_row = Table_expert.Focus.By_row.focused focus in
          let focus_summary =
            [%message "" ~focused:(focused_row : int option)]
            |> Sexp.to_string_hum
            |> fun s -> [%string "%{s}\n"]
          in
          table_to_string
            ~additional_summary:focus_summary
            for_testing
            ~include_stats:false
            ()
        ;;

        let incoming { Table_expert.Result.focus; _ } Action.Focus_down =
          Table.Focus_by_row.focus_down focus
        ;;
      end)
      component
  in
  Handle.show handle;
  [%expect
    {|
    (focused ())
    ┌───┬─────┬────┬────┐
    │ > │ #   │ a  │ b  │
    ├───┼─────┼────┼────┤
    │   │ 0   │ hi │ 1. │
    │   │ 100 │ hi │ 2. │
    │   │ 200 │ hi │ 3. │
    └───┴─────┴────┴────┘
    |}];
  (* See that focus_down with the default rank_range works as intended *)
  Handle.do_actions handle [ Focus_down ];
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    (focused (1))
    ┌───┬─────┬────┬────┐
    │ > │ #   │ a  │ b  │
    ├───┼─────┼────┼────┤
    │ * │ 0   │ hi │ 1. │
    │   │ 100 │ hi │ 2. │
    │   │ 200 │ hi │ 3. │
    └───┴─────┴────┴────┘
    |}];
  Bonsai.Expert.Var.set rank_range (Collate_params.Which_range.Between (3, 5));
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    (focused (1))
    ┌───┬─────┬────┬────┐
    │ > │ #   │ a  │ b  │
    ├───┼─────┼────┼────┤
    │   │ 0   │ hi │ 4. │
    │   │ 100 │ hi │ 5. │
    │   │ 200 │ hi │ 6. │
    └───┴─────┴────┴────┘
    |}];
  (* After setting the [rank_range], focus down no longer focuses an item *)
  Handle.do_actions handle [ Focus_down ];
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    scrolling to index 1 at 40.0px
    (focus_changed_to ())
    (focused ())
    ┌───┬─────┬────┬────┐
    │ > │ #   │ a  │ b  │
    ├───┼─────┼────┼────┤
    │   │ 0   │ hi │ 4. │
    │   │ 100 │ hi │ 5. │
    │   │ 200 │ hi │ 6. │
    └───┴─────┴────┴────┘
    |}];
  (* The focus didn't just stay on the element with b = 1., as demonstrated by performing
     3 more focus down events and viewing the table *)
  Handle.do_actions handle [ Focus_down; Focus_down; Focus_down ];
  Handle.recompute_view_until_stable handle;
  Handle.show handle;
  [%expect
    {|
    scrolling to index 2 at 60.0px
    scrolling to index 3 at 80.0px
    (focus_changed_to (4))
    scrolling to index 4 at 100.0px
    (focus_changed_to (5))
    (focused (5))
    ┌───┬─────┬────┬────┐
    │ > │ #   │ a  │ b  │
    ├───┼─────┼────┼────┤
    │   │ 0   │ hi │ 4. │
    │ * │ 100 │ hi │ 5. │
    │   │ 200 │ hi │ 6. │
    └───┴─────┴────┴────┘
    |}]
;;

let%expect_test "focus down when presence says that all responses are None" =
  let presence ~focus:_ ~collation:_ (local_ _graph) = Bonsai.return None in
  let collate =
    Bonsai.return
      { Incr_map_collate.Collate_params.filter = ()
      ; order = ()
      ; key_range = All_rows
      ; rank_range = All_rows
      ; widen_range_by = 0, 0
      }
  in
  let test =
    Test.create
      ~stats:false
      (Test.Component.expert_for_testing_compute_presence_and_key_rank
         ~collate
         ~presence
         ~key_rank:(fun ~actual_key_rank (local_ _graph) -> actual_key_rank)
         ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows ()))
    ┌───┬─────┬─────┐
    │ > │ #   │ key │
    ├───┼─────┼─────┤
    │   │ 0   │ 0   │
    │   │ 100 │ 1   │
    │   │ 200 │ 4   │
    └───┴─────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  (* notice that visual selection still works, but "focused" remains "()", aka 'none' *)
  [%expect
    {|
    scrolling to index 0 at 0.0px
    ((focused ()) (num_filtered_rows ()))
    ┌───┬─────┬─────┐
    │ > │ #   │ key │
    ├───┼─────┼─────┤
    │ * │ 0   │ 0   │
    │   │ 100 │ 1   │
    │   │ 200 │ 4   │
    └───┴─────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    skipping scroll because target already in view
    ((focused ()) (num_filtered_rows ()))
    ┌───┬─────┬─────┐
    │ > │ #   │ key │
    ├───┼─────┼─────┤
    │   │ 0   │ 0   │
    │ * │ 100 │ 1   │
    │   │ 200 │ 4   │
    └───┴─────┴─────┘
    |}]
;;

let%expect_test "widen_range_by adjusts rows_before and focus index" =
  let presence ~focus ~collation:_ (local_ _graph) = focus in
  let collate =
    Bonsai.return
      { Incr_map_collate.Collate_params.filter = ()
      ; order = ()
      ; key_range = All_rows
      ; rank_range =
          Between (Collate_params.Rank.From_start 5, Collate_params.Rank.From_start 5)
      ; widen_range_by = 2, 1
      }
  in
  let map =
    List.range 0 10
    |> List.map ~f:(fun i ->
      i, { a = "hi"; b = Float.of_int i; c = "c"; d = None; e = "e" })
    |> Int.Map.of_alist_exn
  in
  let test =
    Test.create
      ~stats:true
      ~visible_range:(70, 90)
      ~map
      (Test.Component.expert_for_testing_compute_presence_and_key_rank
         ~collate
         ~presence
         ~key_rank:(fun ~actual_key_rank (local_ _graph) -> actual_key_rank)
         ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows ()))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 3     │
    │ rows-after     │ 4     │
    │ num-filtered   │ 10    │
    │ num-unfiltered │ 10    │
    └────────────────┴───────┘
    ┌───┬─────┬─────┐
    │ > │ #   │ key │
    ├───┼─────┼─────┤
    │   │ 0   │ 3   │
    │   │ 100 │ 4   │
    │   │ 200 │ 5   │
    │   │ 300 │ 6   │
    └───┴─────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_row 2 ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 2 at 20.0px
    ((focused ()) (num_filtered_rows ()))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 3     │
    │ rows-after     │ 4     │
    │ num-filtered   │ 10    │
    │ num-unfiltered │ 10    │
    └────────────────┴───────┘
    ┌───┬─────┬─────┐
    │ > │ #   │ key │
    ├───┼─────┼─────┤
    │   │ 0   │ 3   │
    │   │ 100 │ 4   │
    │   │ 200 │ 5   │
    │   │ 300 │ 6   │
    └───┴─────┴─────┘
    |}]
;;

module%test [@name "focus by key `key_rank` fallback"] _ = struct
  let test () =
    let presence ~focus:_ ~collation:_ (local_ _graph) = Bonsai.return None in
    let collate =
      Bonsai.return
        { Incr_map_collate.Collate_params.filter = ()
        ; order = ()
        ; key_range = To 4
        ; rank_range = All_rows
        ; widen_range_by = 0, 0
        }
    in
    Test.create ~stats:true ~map:big_map ~should_set_bounds:false (fun input filter ->
      let key_rank ~actual_key_rank:_ (local_ graph) =
        let sleep = Bonsai.Clock.sleep graph in
        let%arr sleep in
        fun row_key ->
          let%bind.Effect () = sleep (Time_ns.Span.of_ms 10.) in
          Effect.return (Some ((row_key mod 2) + 2))
      in
      Test.Component.expert_for_testing_compute_presence_and_key_rank
        ~collate
        ~presence
        ~key_rank
        ()
        input
        filter)
  ;;

  let%expect_test "focus by key should fall back to provided `key_rank` if the key isn't \
                   in the current `Collated.t` range"
    =
    let test = test () in
    Handle.recompute_view_until_stable test.handle;
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.do_actions test.handle [ Focus_row 150 ];
    (* It doesn't focus immediately; we have to wait for the effect to complete. *)
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.advance_clock_by test.handle (Time_ns.Span.of_int_ms 10);
    Handle.show test.handle;
    [%expect
      {|
      scrolling to index 2 at 30.0px
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │ * │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}]
  ;;

  let%expect_test "dispatching another focus action while `key_rank` is pending should \
                   'cancel' it."
    =
    let test = test () in
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.do_actions test.handle [ Focus_row 150 ];
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.do_actions test.handle [ Focus_down ];
    Handle.show test.handle;
    [%expect
      {|
      scrolling to index 0 at 0.0px
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │ * │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.advance_clock_by test.handle (Time_ns.Span.of_int_ms 10);
    Handle.recompute_view_until_stable test.handle;
    (* Focus should not have changed, because the pending `key_rank` effect should have
       been "cancelled". *)
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │ * │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}]
  ;;

  let%expect_test "If 2 `key_rank` calls are being processed in parallel, the last one \
                   scheduled should win"
    =
    let test = test () in
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.do_actions test.handle [ Focus_row 150 ];
    Handle.recompute_view_until_stable test.handle;
    Handle.advance_clock_by test.handle (Time_ns.Span.of_int_ms 5);
    Handle.do_actions test.handle [ Focus_row 151 ];
    Handle.recompute_view_until_stable test.handle;
    Handle.advance_clock_by test.handle (Time_ns.Span.of_int_ms 6);
    (* At this point, the first call should have finished, but it's been "cancelled". *)
    Handle.show test.handle;
    [%expect
      {|
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │   │ 300 │ 4   │
      └───┴─────┴─────┘
      |}];
    Handle.advance_clock_by test.handle (Time_ns.Span.of_int_ms 5);
    Handle.recompute_view_until_stable test.handle;
    (* And now, everything should be completed. *)
    Handle.show test.handle;
    [%expect
      {|
      scrolling to index 3 at 40.0px
      ((focused ()) (num_filtered_rows ()))
      ┌────────────────┬───────┐
      │ metric         │ value │
      ├────────────────┼───────┤
      │ rows-before    │ 0     │
      │ rows-after     │ 95    │
      │ num-filtered   │ 99    │
      │ num-unfiltered │ 99    │
      └────────────────┴───────┘
      ┌───┬─────┬─────┐
      │ > │ #   │ key │
      ├───┼─────┼─────┤
      │   │ 0   │ 1   │
      │   │ 100 │ 2   │
      │   │ 200 │ 3   │
      │ * │ 300 │ 4   │
      └───┴─────┴─────┘
      |}]
  ;;
end

let%expect_test "show that scrolling out of a basic table will keep the focus" =
  let test =
    Test.create
      ~stats:true
      ~map:big_map
      ~should_set_bounds:false
      (Test.Component.default ())
  in
  Test.set_bounds test ~low:0 ~high:10;
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 97    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    (focus_changed_to (1))
    ((focused (1)) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 87    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │ * │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 900  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 1000 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 1100 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:3 ~high:13;
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused (1)) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 2     │
    │ rows-after     │ 84    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 900  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 1000 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 1100 │ 12  │ hi │ 6.000000 │ 100 │
    │   │ 1200 │ 13  │ hi │ 6.000000 │ 100 │
    │   │ 1300 │ 14  │ hi │ 7.000000 │ 100 │
    │   │ 1400 │ 15  │ hi │ 7.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Test.set_bounds test ~low:0 ~high:10;
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused (1)) (num_filtered_rows (99)))
    ┌────────────────┬───────┐
    │ metric         │ value │
    ├────────────────┼───────┤
    │ rows-before    │ 0     │
    │ rows-after     │ 87    │
    │ num-filtered   │ 99    │
    │ num-unfiltered │ 99    │
    └────────────────┴───────┘
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │ * │ 0    │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100  │ 2   │ hi │ 1.000000 │ 100 │
    │   │ 200  │ 3   │ hi │ 1.000000 │ 100 │
    │   │ 300  │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 400  │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 500  │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 600  │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 700  │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 800  │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 900  │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 1000 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 1100 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "show that scrolling out of a custom table will execute the presence \
                 component"
  =
  let open Incr_map_collate.Collate_params.Which_range in
  let presence ~focus ~collation (local_ _graph) =
    let%arr focus and collation in
    match focus with
    | None -> None
    | Some focus ->
      if Map.exists (Incr_map_collate.Collated.to_opaque_map collation) ~f:(fun (k, _v) ->
           focus = k)
      then Some focus
      else None
  in
  let rank = Bonsai.Expert.Var.create (Between (0, 10)) in
  let collate =
    let%map rank_range = Bonsai.Expert.Var.value rank in
    { Incr_map_collate.Collate_params.Stable.V1.filter = ()
    ; order = ()
    ; key_range = All_rows
    ; rank_range
    }
    |> Incr_map_collate.Collate_params.of_stable_v1
  in
  let test =
    Test.create
      ~map:big_map
      ~stats:false
      (Test.Component.expert_for_testing_compute_presence_and_key_rank
         ~collate
         ~presence
         ~key_rank:(fun ~actual_key_rank (local_ _graph) -> actual_key_rank)
         ())
  in
  Bonsai.Expert.Var.set rank (Between (0, 10));
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows ()))
    ┌───┬──────┬─────┐
    │ > │ #    │ key │
    ├───┼──────┼─────┤
    │   │ 0    │ 1   │
    │   │ 100  │ 2   │
    │   │ 200  │ 3   │
    │   │ 300  │ 4   │
    │   │ 400  │ 5   │
    │   │ 500  │ 6   │
    │   │ 600  │ 7   │
    │   │ 700  │ 8   │
    │   │ 800  │ 9   │
    │   │ 900  │ 10  │
    │   │ 1000 │ 11  │
    └───┴──────┴─────┘
    |}];
  Handle.do_actions test.handle [ Focus_down ];
  Handle.show test.handle;
  [%expect
    {|
    scrolling to index 0 at 0.0px
    ((focused (1)) (num_filtered_rows ()))
    ┌───┬──────┬─────┐
    │ > │ #    │ key │
    ├───┼──────┼─────┤
    │ * │ 0    │ 1   │
    │   │ 100  │ 2   │
    │   │ 200  │ 3   │
    │   │ 300  │ 4   │
    │   │ 400  │ 5   │
    │   │ 500  │ 6   │
    │   │ 600  │ 7   │
    │   │ 700  │ 8   │
    │   │ 800  │ 9   │
    │   │ 900  │ 10  │
    │   │ 1000 │ 11  │
    └───┴──────┴─────┘
    |}];
  Bonsai.Expert.Var.set rank (Between (3, 13));
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  (* notice that when we scrolled away, the "focused" value is set to None. *)
  [%expect
    {|
    ((focused ()) (num_filtered_rows ()))
    ┌───┬──────┬─────┐
    │ > │ #    │ key │
    ├───┼──────┼─────┤
    │   │ 300  │ 4   │
    │   │ 400  │ 5   │
    │   │ 500  │ 6   │
    │   │ 600  │ 7   │
    │   │ 700  │ 8   │
    │   │ 800  │ 9   │
    │   │ 900  │ 10  │
    │   │ 1000 │ 11  │
    │   │ 1100 │ 12  │
    │   │ 1200 │ 13  │
    │   │ 1300 │ 14  │
    └───┴──────┴─────┘
    |}];
  Bonsai.Expert.Var.set rank (Between (0, 10));
  Handle.recompute_view_until_stable test.handle;
  Handle.show test.handle;
  [%expect
    {|
    ((focused (1)) (num_filtered_rows ()))
    ┌───┬──────┬─────┐
    │ > │ #    │ key │
    ├───┼──────┼─────┤
    │ * │ 0    │ 1   │
    │   │ 100  │ 2   │
    │   │ 200  │ 3   │
    │   │ 300  │ 4   │
    │   │ 400  │ 5   │
    │   │ 500  │ 6   │
    │   │ 600  │ 7   │
    │   │ 700  │ 8   │
    │   │ 800  │ 9   │
    │   │ 900  │ 10  │
    │   │ 1000 │ 11  │
    └───┴──────┴─────┘
    |}]
;;

let%expect_test "what happens if the table body goes entirely off screen" =
  let test =
    Test.create
      ~stats:false
      ~map:big_map
      ~visible_range:(5, 10)
      (Test.Component.default ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Test.clear_bounds test;
  Handle.show_diff test.handle;
  [%expect {| |}];
  Handle.show test.handle;
  (* This test does not fully demonstrate that we handle this situation correctly, since
     it doesn't show how anything will affect the browser's scroll position. However, it
     does demonstrate what happens, which is still useful. *)
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 1   │ hi │ 0.000000 │ 100 │
    │   │ 100 │ 2   │ hi │ 1.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}]
;;

let%expect_test "dynamic row height" =
  let row_height_var = Bonsai.Expert.Var.create (`Px 1) in
  let test =
    Test.create
      ~stats:false
      ~map:big_map
      ~visible_range:(5, 10)
      (Test.Component.default ~row_height:(Bonsai.Expert.Var.value row_height_var) ())
  in
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set row_height_var (`Px 2);
  Handle.show test.handle;
  [%expect
    {|
    scrolling position 14.px into view
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set row_height_var (`Px 0);
  Handle.show test.handle;
  [%expect
    {|
    scrolling position 3.px into view
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ -100 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 0    │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100  │ 6   │ hi │ 3.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬──────┬─────┬────┬──────────┬─────┐
    │ > │ #    │ key │ a  │ b        │ d   │
    ├───┼──────┼─────┼────┼──────────┼─────┤
    │   │ -200 │ 3   │ hi │ 1.000000 │ 100 │
    │   │ -100 │ 4   │ hi │ 2.000000 │ 100 │
    │   │ 0    │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100  │ 6   │ hi │ 3.000000 │ 100 │
    └───┴──────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set row_height_var (`Px 10000);
  Handle.show test.handle;
  [%expect
    {|
    scrolling position 50004.px into view
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬─────┬─────┬────┬──────────┬─────┐
    │ > │ #   │ key │ a  │ b        │ d   │
    ├───┼─────┼─────┼────┼──────────┼─────┤
    │   │ 0   │ 5   │ hi │ 2.000000 │ 100 │
    │   │ 100 │ 6   │ hi │ 3.000000 │ 100 │
    │   │ 200 │ 7   │ hi │ 3.000000 │ 100 │
    │   │ 300 │ 8   │ hi │ 4.000000 │ 100 │
    │   │ 400 │ 9   │ hi │ 4.000000 │ 100 │
    │   │ 500 │ 10  │ hi │ 5.000000 │ 100 │
    │   │ 600 │ 11  │ hi │ 5.000000 │ 100 │
    │   │ 700 │ 12  │ hi │ 6.000000 │ 100 │
    └───┴─────┴─────┴────┴──────────┴─────┘
    |}];
  Bonsai.Expert.Var.set row_height_var (`Px (-10));
  Handle.show test.handle;
  [%expect
    {|
    scrolling position 4.px into view
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬───┬─────┬────┬──────────┬─────┐
    │ > │ # │ key │ a  │ b        │ d   │
    ├───┼───┼─────┼────┼──────────┼─────┤
    │   │ 0 │ 1   │ hi │ 0.000000 │ 100 │
    └───┴───┴─────┴────┴──────────┴─────┘
    |}];
  Handle.show test.handle;
  [%expect
    {|
    ((focused ()) (num_filtered_rows (99)))
    ┌───┬───┬─────┬────┬──────────┬─────┐
    │ > │ # │ key │ a  │ b        │ d   │
    ├───┼───┼─────┼────┼──────────┼─────┤
    │   │ 0 │ 1   │ hi │ 0.000000 │ 100 │
    └───┴───┴─────┴────┴──────────┴─────┘
    |}]
;;

module%test [@name "dynamic columns with visibility"] _ = struct
  let setup ~visibility_starts_out_as =
    let visibility_of_first_column = Bonsai.Expert.Var.create visibility_starts_out_as in
    let map =
      [ 1; 2; 3; 4; 5; 6; 7 ]
      |> List.map ~f:(fun i -> i, i)
      |> Int.Map.of_alist_exn
      |> Bonsai.return
    in
    let component (local_ graph) =
      let collate, _ =
        let collate =
          { Collate_params.filter = None
          ; order = Compare.Unchanged
          ; key_range = Collate_params.Which_range.All_rows
          ; rank_range = Collate_params.Which_range.All_rows
          ; widen_range_by = 0, 0
          }
        in
        Table_expert.collate
          ~filter_equal:phys_equal
          ~filter_to_predicate:Fn.id
          ~order_equal:phys_equal
          ~order_to_compare:Fn.id
          map
          (Bonsai.return collate)
          graph
      in
      let%sub { view; _ } =
        Table_expert.component
          (module Int)
          ~focus:Table_expert.Focus.None
          ~row_height:(Bonsai.return (`Px 20))
          ~columns:
            ((let%map visibility_of_first_column =
                Bonsai.Expert.Var.value visibility_of_first_column
              in
              [ Table_expert.Columns.Dynamic_columns.column
                  ~header:(Vdom.Node.text "a")
                  ~cell:(fun ~key:_ ~data -> Vdom.Node.textf "%d" data)
                  ~visible:visibility_of_first_column
                  ()
              ])
             |> Table_expert.Columns.Dynamic_columns.lift)
          collate
          graph
      in
      view
    in
    component, Bonsai.Expert.Var.set visibility_of_first_column
  ;;

  let%expect_test "REGRESSION: starting a column as invisible shouldn't crash" =
    let component, _set_visibility = setup ~visibility_starts_out_as:false in
    let (_ : _ Handle.t) = Handle.create (Result_spec.vdom Fn.id) component in
    ();
    [%expect {| |}]
  ;;

  let%expect_test "REGRESSION: toggling a column to be invisible shouldn't crash" =
    let component, set_visibility = setup ~visibility_starts_out_as:true in
    let handle = Handle.create (Result_spec.vdom Fn.id) component in
    set_visibility false;
    Handle.recompute_view handle;
    [%expect {| |}]
  ;;
end
