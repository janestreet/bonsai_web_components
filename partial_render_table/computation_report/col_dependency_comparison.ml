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
    │ Update col dep 2 times: 10 rows     │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 100 rows    │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 101 rows    │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 1000 rows   │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 10000 rows  │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 10 times: 10 rows    │  567                     │  567                      │  567                    │ 412                     │ 412                      │ 412                    │
    │ Update col dep 10 times: 100 rows   │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 101 rows   │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 1000 rows  │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 10000 rows │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
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
    │ Update col dep 10 times: 10 rows    │  318                     │  318                      │  318                    │ 174                     │ 174                      │ 174                    │
    │ Update col dep 10 times: 100 rows   │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 101 rows   │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 1000 rows  │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 10000 rows │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   55                    │   55                     │   55                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   55                    │   55                     │   55                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   55                    │   55                     │   55                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   55                    │   55                     │   55                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   55                    │   55                     │   55                   │
    │ Update col dep 10 times: 10 rows    │ 1416                     │ 1523                      │ 1631                    │ 1077                    │ 1077                     │ 1077                   │
    │ Update col dep 10 times: 100 rows   │ 7057                     │ 7165                      │ 7273                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 101 rows   │ 7057                     │ 7165                      │ 7273                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 1000 rows  │ 7057                     │ 7165                      │ 7273                    │ 3805                    │ 3805                     │ 3805                   │
    │ Update col dep 10 times: 10000 rows │ 7057                     │ 7165                      │ 7273                    │ 3805                    │ 3805                     │ 3805                   │
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
    │ Update col dep 10 times: 10 rows    │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 100 rows   │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 101 rows   │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 1000 rows  │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 10000 rows │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
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
    │ Update col dep 2 times: 10 rows     │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 100 rows    │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 101 rows    │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 1000 rows   │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 2 times: 10000 rows  │  377                     │  377                      │  377                    │ 330                     │ 330                      │ 330                    │
    │ Update col dep 10 times: 10 rows    │  567                     │  567                      │  567                    │ 412                     │ 412                      │ 412                    │
    │ Update col dep 10 times: 100 rows   │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 101 rows   │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 1000 rows  │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
    │ Update col dep 10 times: 10000 rows │ 1869                     │ 1869                      │ 1869                    │ 970                     │ 970                      │ 970                    │
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
    │ Update col dep 10 times: 10 rows    │  318                     │  318                      │  318                    │ 174                     │ 174                      │ 174                    │
    │ Update col dep 10 times: 100 rows   │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 101 rows   │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 1000 rows  │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    │ Update col dep 10 times: 10000 rows │ 2178                     │ 2178                      │ 2178                    │ 980                     │ 980                      │ 980                    │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘

    ====== Nodes Recomputed ======
    ┌─────────────────────────────────────┬──────────────────────────┬───────────────────────────┬─────────────────────────┬─────────────────────────┬──────────────────────────┬────────────────────────┐
    │                                     │ dynamic cells: first col │ dynamic cells: middle col │ dynamic cells: last col │ dynamic cols: first col │ dynamic cols: middle col │ dynamic cols: last col │
    ├─────────────────────────────────────┼──────────────────────────┼───────────────────────────┼─────────────────────────┼─────────────────────────┼──────────────────────────┼────────────────────────┤
    │ Update col dep 2 times: 10 rows     │   42                     │   54                      │   66                    │   56                    │   56                     │   56                   │
    │ Update col dep 2 times: 100 rows    │   42                     │   54                      │   66                    │   56                    │   56                     │   56                   │
    │ Update col dep 2 times: 101 rows    │   42                     │   54                      │   66                    │   56                    │   56                     │   56                   │
    │ Update col dep 2 times: 1000 rows   │   42                     │   54                      │   66                    │   56                    │   56                     │   56                   │
    │ Update col dep 2 times: 10000 rows  │   42                     │   54                      │   66                    │   56                    │   56                     │   56                   │
    │ Update col dep 10 times: 10 rows    │ 1416                     │ 1523                      │ 1631                    │ 1086                    │ 1086                     │ 1086                   │
    │ Update col dep 10 times: 100 rows   │ 7057                     │ 7165                      │ 7273                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 101 rows   │ 7057                     │ 7165                      │ 7273                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 1000 rows  │ 7057                     │ 7165                      │ 7273                    │ 3814                    │ 3814                     │ 3814                   │
    │ Update col dep 10 times: 10000 rows │ 7057                     │ 7165                      │ 7273                    │ 3814                    │ 3814                     │ 3814                   │
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
    │ Update col dep 10 times: 10 rows    │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 100 rows   │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 101 rows   │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 1000 rows  │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    │ Update col dep 10 times: 10000 rows │ 48                       │ 48                        │ 48                      │ 57                      │ 57                       │ 57                     │
    └─────────────────────────────────────┴──────────────────────────┴───────────────────────────┴─────────────────────────┴─────────────────────────┴──────────────────────────┴────────────────────────┘
    |}]
;;
