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
    │ Update col dep 2 times: 10 rows     │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 100 rows    │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 101 rows    │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 1000 rows   │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 10000 rows  │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 10 times: 10 rows    │  556                     │  556                      │  556                    │ 401                     │ 401                      │ 401                    │
    │ Update col dep 10 times: 100 rows   │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 101 rows   │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 1000 rows  │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 10000 rows │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
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
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   57                    │   57                     │   57                   │
    │ Update col dep 10 times: 10 rows    │ 1394                     │ 1501                      │ 1609                    │ 1073                    │ 1073                     │ 1073                   │
    │ Update col dep 10 times: 100 rows   │ 7035                     │ 7143                      │ 7251                    │ 3801                    │ 3801                     │ 3801                   │
    │ Update col dep 10 times: 101 rows   │ 7035                     │ 7143                      │ 7251                    │ 3801                    │ 3801                     │ 3801                   │
    │ Update col dep 10 times: 1000 rows  │ 7035                     │ 7143                      │ 7251                    │ 3801                    │ 3801                     │ 3801                   │
    │ Update col dep 10 times: 10000 rows │ 7035                     │ 7143                      │ 7251                    │ 3801                    │ 3801                     │ 3801                   │
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
    │ Update col dep 2 times: 10 rows     │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 100 rows    │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 101 rows    │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 1000 rows   │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 2 times: 10000 rows  │  370                     │  370                      │  370                    │ 323                     │ 323                      │ 323                    │
    │ Update col dep 10 times: 10 rows    │  556                     │  556                      │  556                    │ 401                     │ 401                      │ 401                    │
    │ Update col dep 10 times: 100 rows   │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 101 rows   │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 1000 rows  │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
    │ Update col dep 10 times: 10000 rows │ 1858                     │ 1858                      │ 1858                    │ 959                     │ 959                      │ 959                    │
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
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   58                    │   58                     │   58                   │
    │ Update col dep 10 times: 10 rows    │ 1394                     │ 1501                      │ 1609                    │ 1082                    │ 1082                     │ 1082                   │
    │ Update col dep 10 times: 100 rows   │ 7035                     │ 7143                      │ 7251                    │ 3810                    │ 3810                     │ 3810                   │
    │ Update col dep 10 times: 101 rows   │ 7035                     │ 7143                      │ 7251                    │ 3810                    │ 3810                     │ 3810                   │
    │ Update col dep 10 times: 1000 rows  │ 7035                     │ 7143                      │ 7251                    │ 3810                    │ 3810                     │ 3810                   │
    │ Update col dep 10 times: 10000 rows │ 7035                     │ 7143                      │ 7251                    │ 3810                    │ 3810                     │ 3810                   │
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
