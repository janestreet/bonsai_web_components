open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
open! Incr_map_collate
open Bonsai_web_ui_partial_render_table_configs_for_testing

let startup =
  Bonsai_bench.compare_startup
    ~name:"PRT Startup"
    ~print_separate_rows:true
    ~computations:(force All_apis_configs.full_power_comparison_sorted_for_bench)
    (Symbol_table.startup_inputs
       [ 0; 1; 10; 100; 101; 1_000; 10_000; 100_000; 1_000_000 ])
;;

let interaction_all =
  Bonsai_bench.compare_interactions
    ~name:"Interaction Benchmarking: All APIs"
    ~print_separate_rows:true
    ~get_inject:All_apis_configs.get_inject
    ~computations:(force All_apis_configs.full_power_comparison_sorted_for_bench)
    Symbol_table.scenarios
;;

let interaction_changing_column_dependencies =
  Bonsai_bench.compare_interactions
    ~name:"Interaction Benchmarking: Changing Column Dependencies"
    ~print_separate_rows:true
    ~get_inject:(fun _ _ -> assert false)
    ~computations:(force Col_dependency_configs.all_computations)
    Col_dependency_configs.scenarios
;;

let () =
  Bonsai_bench.run_sets_via_command
    [ startup; interaction_all; interaction_changing_column_dependencies ]
;;
