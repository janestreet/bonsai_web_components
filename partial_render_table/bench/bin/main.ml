open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Bonsai_bench
open! Bonsai_web_ui_partial_render_table_configs_for_testing
open! Incr_map_collate

let () = print_endline "======== Startup Benchmarking ========"

module Config = struct
  include Config

  let name = function
    | New_api { render_cell_kind = Stateful_cells; cols = Dynamic; _ } ->
      "new (incr cells) (dynamic)"
    | New_api { render_cell_kind = Stateful_rows; cols = Dynamic; _ } ->
      "new (incr rows) (dynamic)"
    | New_api { render_cell_kind = Pure; cols = Dynamic; _ } -> "new (pure) (dynamic)"
    | New_api { render_cell_kind = Stateful_cells; cols = Static; _ } ->
      "new (incr cells) (static)"
    | New_api { render_cell_kind = Stateful_rows; cols = Static; _ } ->
      "new (incr rows) (static)"
    | New_api { render_cell_kind = Pure; cols = Static; _ } -> "new (pure) (static)"
    | Dynamic_cells _ -> "dyn cells"
    | Dynamic_cols _ -> "dyn cols"
    | Dynamic_experimental _ | New_api { cols = Dynamic_constant_foldable; _ } ->
      "NOT TESTED"
  ;;
end

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  let inputs =
    List.map [ 0; 1; 10; 100; 101; 1_000; 10_000; 100_000; 1_000_000 ] ~f:(fun n ->
      let input = Prt_input.create (Row.init_rows n) in
      Int.to_string n, input)
  in
  Bonsai_bench.benchmark_compare_startup
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    (module Config)
    ~inputs
    ~configs:Config.full_power_comparison
;;

let () = print_endline "======== Performance Benchmarking ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark_compare_interactions
    ~print_separate_rows:true
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    (module Config)
    ~scenarios
    ~configs:Config.full_power_comparison
;;
