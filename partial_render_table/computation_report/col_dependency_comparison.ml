open! Core
open Bonsai_web_partial_render_table_configs_for_testing
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
    │ Update col dep 2 times: 10 rows     │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 100 rows    │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 101 rows    │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 1000 rows   │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 10000 rows  │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 10 times: 10 rows    │  557                     │  557                      │  557                    │ 402                     │ 402                      │ 402                    │
    │ Update col dep 10 times: 100 rows   │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 101 rows   │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 1000 rows  │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 10000 rows │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
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
    │ Update col dep 10 times: 10 rows    │  292                     │  292                      │  292                    │ 157                     │ 157                      │ 157                    │
    │ Update col dep 10 times: 100 rows   │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 101 rows   │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 1000 rows  │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 10000 rows │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   43                     │   55                      │   67                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 100 rows    │   43                     │   55                      │   67                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 101 rows    │   43                     │   55                      │   67                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 1000 rows   │   43                     │   55                      │   67                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 10000 rows  │   43                     │   55                      │   67                    │   58                    │   58                     │   58                   │
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
    │ Update col dep 10 times: 10 rows    │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 100 rows   │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 101 rows   │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 1000 rows  │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 10000 rows │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
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
    │ Update col dep 2 times: 10 rows     │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 100 rows    │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 101 rows    │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 1000 rows   │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 2 times: 10000 rows  │  371                     │  371                      │  371                    │ 324                     │ 324                      │ 324                    │
    │ Update col dep 10 times: 10 rows    │  557                     │  557                      │  557                    │ 402                     │ 402                      │ 402                    │
    │ Update col dep 10 times: 100 rows   │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 101 rows   │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 1000 rows  │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
    │ Update col dep 10 times: 10000 rows │ 1859                     │ 1859                      │ 1859                    │ 960                     │ 960                      │ 960                    │
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
    │ Update col dep 10 times: 10 rows    │  292                     │  292                      │  292                    │ 157                     │ 157                      │ 157                    │
    │ Update col dep 10 times: 100 rows   │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 101 rows   │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 1000 rows  │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    │ Update col dep 10 times: 10000 rows │ 2090                     │ 2090                      │ 2090                    │ 963                     │ 963                      │ 963                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   43                     │   55                      │   67                    │   59                    │   59                     │   59                   │
    │ Update col dep 2 times: 100 rows    │   43                     │   55                      │   67                    │   59                    │   59                     │   59                   │
    │ Update col dep 2 times: 101 rows    │   43                     │   55                      │   67                    │   59                    │   59                     │   59                   │
    │ Update col dep 2 times: 1000 rows   │   43                     │   55                      │   67                    │   59                    │   59                     │   59                   │
    │ Update col dep 2 times: 10000 rows  │   43                     │   55                      │   67                    │   59                    │   59                     │   59                   │
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
    │ Update col dep 10 times: 10 rows    │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 100 rows   │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 101 rows   │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 1000 rows  │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    │ Update col dep 10 times: 10000 rows │ 35                       │ 35                        │ 35                      │ 44                      │ 44                       │ 44                     │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘
    |}]
;;
