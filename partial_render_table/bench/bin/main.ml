open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
open! Incr_map_collate
open Bonsai_web_ui_partial_render_table_configs_for_testing

let () = print_endline "======== Startup Benchmarking ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark_compare_startup
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    ~computations:(force All_apis_configs.full_power_comparison_sorted_for_bench)
    (Symbol_table.startup_inputs
       [ 0; 1; 10; 100; 101; 1_000; 10_000; 100_000; 1_000_000 ])
;;

let () = print_endline "======== Interaction Benchmarking: All APIs ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark_compare_interactions
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    ~get_inject:All_apis_configs.get_inject
    ~computations:(force All_apis_configs.full_power_comparison_sorted_for_bench)
    Symbol_table.scenarios
;;

let () =
  print_endline "======== Interaction Benchmarking: Changing Column Dependencies ========"
;;

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark_compare_interactions
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    ~get_inject:(fun _ _ -> assert false)
    ~computations:(force Col_dependency_configs.all_computations)
    Col_dependency_configs.scenarios
;;
