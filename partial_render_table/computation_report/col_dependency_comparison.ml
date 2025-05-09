open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = Col_dependency_configs
module Report = Bonsai_web_test.Computation_report

(* This test compares tables where only one column depends on a changing `Bonsai.t`. *)

let%expect_test ("JavaScript" [@tags "no-wasm"]) =
  Report.Interaction.run_and_print_compare
    ~get_inject:(fun _ _ -> assert false)
    ~computations:(force Config.all_computations)
    Config.scenarios;
  [%expect
    {|
    ====== Node Count ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 100 rows    │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 101 rows    │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 1000 rows   │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 10000 rows  │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 10 times: 10 rows    │  555                     │  555                      │  555                    │ 400                     │ 400                      │ 400                    │
    │ Update col dep 10 times: 100 rows   │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 101 rows   │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 1000 rows  │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 10000 rows │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Created ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 100 rows    │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 101 rows    │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 1000 rows   │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 10000 rows  │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 10 times: 10 rows    │  306                     │  306                      │  306                    │ 162                     │ 162                      │ 162                    │
    │ Update col dep 10 times: 100 rows   │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 101 rows   │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 1000 rows  │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 10000 rows │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 10 times: 10 rows    │ 1404                     │ 1511                      │ 1619                    │ 1083                    │ 1083                     │ 1083                   │
    │ Update col dep 10 times: 100 rows   │ 7045                     │ 7153                      │ 7261                    │ 3811                    │ 3811                     │ 3811                   │
    │ Update col dep 10 times: 101 rows   │ 7045                     │ 7153                      │ 7261                    │ 3811                    │ 3811                     │ 3811                   │
    │ Update col dep 10 times: 1000 rows  │ 7045                     │ 7153                      │ 7261                    │ 3811                    │ 3811                     │ 3811                   │
    │ Update col dep 10 times: 10000 rows │ 7045                     │ 7153                      │ 7261                    │ 3811                    │ 3811                     │ 3811                   │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Invalidated ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 100 rows    │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 101 rows    │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 1000 rows   │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 10000 rows  │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 10 times: 10 rows    │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 100 rows   │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 101 rows   │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 1000 rows  │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 10000 rows │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘
    |}]
;;

let%expect_test ("WASM" [@tags "wasm-only"]) =
  Report.Interaction.run_and_print_compare
    ~get_inject:(fun _ _ -> assert false)
    ~computations:(force Config.all_computations)
    Config.scenarios;
  [%expect
    {|
    ====== Node Count ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 100 rows    │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 101 rows    │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 1000 rows   │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 2 times: 10000 rows  │  368                     │  368                      │  368                    │ 321                     │ 321                      │ 321                    │
    │ Update col dep 10 times: 10 rows    │  555                     │  555                      │  555                    │ 400                     │ 400                      │ 400                    │
    │ Update col dep 10 times: 100 rows   │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 101 rows   │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 1000 rows  │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    │ Update col dep 10 times: 10000 rows │ 1857                     │ 1857                      │ 1857                    │ 958                     │ 958                      │ 958                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Created ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 100 rows    │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 101 rows    │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 1000 rows   │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 2 times: 10000 rows  │    0                     │    0                      │    0                    │   1                     │   1                      │   1                    │
    │ Update col dep 10 times: 10 rows    │  306                     │  306                      │  306                    │ 162                     │ 162                      │ 162                    │
    │ Update col dep 10 times: 100 rows   │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 101 rows   │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 1000 rows  │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    │ Update col dep 10 times: 10000 rows │ 2166                     │ 2166                      │ 2166                    │ 968                     │ 968                      │ 968                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 10 times: 10 rows    │ 1404                     │ 1511                      │ 1619                    │ 1092                    │ 1092                     │ 1092                   │
    │ Update col dep 10 times: 100 rows   │ 7045                     │ 7153                      │ 7261                    │ 3820                    │ 3820                     │ 3820                   │
    │ Update col dep 10 times: 101 rows   │ 7045                     │ 7153                      │ 7261                    │ 3820                    │ 3820                     │ 3820                   │
    │ Update col dep 10 times: 1000 rows  │ 7045                     │ 7153                      │ 7261                    │ 3820                    │ 3820                     │ 3820                   │
    │ Update col dep 10 times: 10000 rows │ 7045                     │ 7153                      │ 7261                    │ 3820                    │ 3820                     │ 3820                   │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Invalidated ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 100 rows    │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 101 rows    │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 1000 rows   │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 2 times: 10000 rows  │  0                       │  0                        │  0                      │  1                      │  1                       │  1                     │
    │ Update col dep 10 times: 10 rows    │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 100 rows   │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 101 rows   │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 1000 rows  │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    │ Update col dep 10 times: 10000 rows │ 39                       │ 39                        │ 39                      │ 48                      │ 48                       │ 48                     │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘
    |}]
;;
