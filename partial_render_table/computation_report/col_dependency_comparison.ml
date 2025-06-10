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
    │ Update col dep 2 times: 10 rows     │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 100 rows    │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 101 rows    │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 1000 rows   │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 10000 rows  │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 10 times: 10 rows    │  550                     │  550                      │  550                    │ 395                     │ 395                      │ 395                    │
    │ Update col dep 10 times: 100 rows   │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 101 rows   │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 1000 rows  │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 10000 rows │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
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
    │ Update col dep 10 times: 10 rows    │  305                     │  305                      │  305                    │ 161                     │ 161                      │ 161                    │
    │ Update col dep 10 times: 100 rows   │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 101 rows   │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 1000 rows  │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 10000 rows │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
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
    │ Update col dep 10 times: 10 rows    │ 1398                     │ 1505                      │ 1613                    │ 1077                    │ 1077                     │ 1077                   │
    │ Update col dep 10 times: 100 rows   │ 7039                     │ 7147                      │ 7255                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 101 rows   │ 7039                     │ 7147                      │ 7255                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 1000 rows  │ 7039                     │ 7147                      │ 7255                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 10000 rows │ 7039                     │ 7147                      │ 7255                    │ 3805                    │ 3805                     │ 3805                   │
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
    │ Update col dep 2 times: 10 rows     │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 100 rows    │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 101 rows    │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 1000 rows   │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 2 times: 10000 rows  │  364                     │  364                      │  364                    │ 317                     │ 317                      │ 317                    │
    │ Update col dep 10 times: 10 rows    │  550                     │  550                      │  550                    │ 395                     │ 395                      │ 395                    │
    │ Update col dep 10 times: 100 rows   │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 101 rows   │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 1000 rows  │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
    │ Update col dep 10 times: 10000 rows │ 1852                     │ 1852                      │ 1852                    │ 953                     │ 953                      │ 953                    │
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
    │ Update col dep 10 times: 10 rows    │  305                     │  305                      │  305                    │ 161                     │ 161                      │ 161                    │
    │ Update col dep 10 times: 100 rows   │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 101 rows   │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 1000 rows  │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
    │ Update col dep 10 times: 10000 rows │ 2165                     │ 2165                      │ 2165                    │ 967                     │ 967                      │ 967                    │
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
    │ Update col dep 10 times: 10 rows    │ 1398                     │ 1505                      │ 1613                    │ 1086                    │ 1086                     │ 1086                   │
    │ Update col dep 10 times: 100 rows   │ 7039                     │ 7147                      │ 7255                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 101 rows   │ 7039                     │ 7147                      │ 7255                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 1000 rows  │ 7039                     │ 7147                      │ 7255                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 10000 rows │ 7039                     │ 7147                      │ 7255                    │ 3814                    │ 3814                     │ 3814                   │
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
