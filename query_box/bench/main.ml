open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
module Query_box = Bonsai_web_ui_query_box

let component ~possible_words (local_ graph) =
  Query_box.stringable
    (module String)
    ~filter_strategy:Fuzzy_search_and_score
    ~on_select:(return (fun _ -> Effect.Ignore))
    possible_words
    graph
;;

let all_words = lazy (In_channel.read_lines "/usr/share/dict/words" |> Array.of_list)

let get_random_set_of_words ~n =
  let all_words = force all_words in
  let map = ref String.Map.empty in
  for _ = 1 to n do
    let i = Random.int (Array.length all_words) in
    let s = Array.nget all_words i in
    map := Map.set !map ~key:s ~data:s
  done;
  !map
;;

let startup =
  List.map [ 1; 100; 10_000; 1_000_000 ] ~f:(fun num_words ->
    let possible_words = get_random_set_of_words ~n:num_words |> return in
    Bonsai_bench.create_for_startup
      ~name:[%string "querybox with %{num_words#Int} options"]
      (component ~possible_words))
;;

let get_random_query () =
  let generator =
    let%bind.Core.Quickcheck.Generator length = Int.gen_incl 1 3 in
    String.gen_with_length length Char.gen_lowercase
  in
  Quickcheck.random_value generator
;;

let interaction_changing_query =
  List.map [ 1; 100; 10_000; 1_000_000 ] ~f:(fun num_words ->
    let possible_words = get_random_set_of_words ~n:num_words |> return in
    let interaction =
      let query_changes =
        List.init 10 ~f:(fun _ -> Interaction.inject (`Set_query (get_random_query ())))
      in
      Interaction.inject `Activate :: query_changes |> Interaction.many_with_recomputes
    in
    Bonsai_bench.create
      ~name:[%string "changing query 10 times with %{num_words#Int} options"]
      ~component:(component ~possible_words)
      ~get_inject:(fun query_box action ->
        match action with
        | `Set_query query -> Query_box.set_query query_box query
        | `Activate -> Query_box.activate_for_benchmarking query_box)
      interaction)
;;

let () =
  Bonsai_bench.run_sets_via_command
    [ Bonsai_bench.set ~name:"Startup" startup
    ; Bonsai_bench.set ~name:"Changing Query" interaction_changing_query
    ]
;;

(** {v
 JS:
======== Startup Benchmarking ========
Estimated testing time 4s (4 benchmarks x 1s). Change using '-quota'.
┌───────────────────────────────┬──────────┬────────────┐
│ Name                          │ Time/Run │ Percentage │
├───────────────────────────────┼──────────┼────────────┤
│ querybox with 1 options       │   1.77ms │    100.00% │
│ querybox with 100 options     │   1.44ms │     81.39% │
│ querybox with 10000 options   │   1.52ms │     85.88% │
│ querybox with 1000000 options │   1.58ms │     88.98% │
└───────────────────────────────┴──────────┴────────────┘

======== Interaction benchmarking: changing query ========
Estimated testing time 4s (4 benchmarks x 1s). Change using '-quota'.
┌──────────────────────────────────────────────┬────────────┬────────────┐
│ Name                                         │   Time/Run │ Percentage │
├──────────────────────────────────────────────┼────────────┼────────────┤
│ changing query 10 times with 1 options       │     2.21ms │      0.03% │
│ changing query 10 times with 100 options     │     3.44ms │      0.05% │
│ changing query 10 times with 10000 options   │    91.83ms │      1.24% │
│ changing query 10 times with 1000000 options │ 7_428.59ms │    100.00% │
└──────────────────────────────────────────────┴────────────┴────────────┘
    v} *)

(** {v
 Wasm:
======== Startup Benchmarking ========
Estimated testing time 4s (4 benchmarks x 1s). Change using '-quota'.
┌───────────────────────────────┬──────────┬────────────┐
│ Name                          │ Time/Run │ Percentage │
├───────────────────────────────┼──────────┼────────────┤
│ querybox with 1 options       │ 959.05us │     99.91% │
│ querybox with 100 options     │ 847.62us │     88.30% │
│ querybox with 10000 options   │ 959.91us │    100.00% │
│ querybox with 1000000 options │ 838.60us │     87.36% │
└───────────────────────────────┴──────────┴────────────┘

======== Interaction benchmarking: changing query ========
Estimated testing time 4s (4 benchmarks x 1s). Change using '-quota'.
┌──────────────────────────────────────────────┬────────────────┬────────────┐
│ Name                                         │       Time/Run │ Percentage │
├──────────────────────────────────────────────┼────────────────┼────────────┤
│ changing query 10 times with 1 options       │       775.23us │      0.02% │
│ changing query 10 times with 100 options     │     1_722.92us │      0.05% │
│ changing query 10 times with 10000 options   │    37_339.64us │      1.03% │
│ changing query 10 times with 1000000 options │ 3_617_258.36us │    100.00% │
└──────────────────────────────────────────────┴────────────────┴────────────┘
    v} *)
