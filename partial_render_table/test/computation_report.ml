open! Core
open Shared_with_bench
module Report = Bonsai_web_test.Computation_report

module%test [@name "startup"] _ = struct
  let test_startup configs =
    let startup_inputs =
      List.map [ 100; 100_000 ] ~f:(fun size ->
        Int.to_string size, Prt_input.create (Row.init_rows size))
    in
    Report.Startup.run_and_print_compare (module Config) startup_inputs configs
  ;;

  let%expect_test "Flat" =
    test_startup Config.all_flat;
    [%expect
      {|
      ======= Startup Incr Node Stats =======
      ┌─────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
      │                                     │ max_height │ node_count │ max_node_id │ nodes_created │
      ├─────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
      │ dyn cells (stateless)(flat): 100    │  87        │  1094      │  1530       │  1531         │
      │ dyn cells (state)(flat): 100        │ 103        │ 12343      │ 15600       │ 15601         │
      │ dyn cols(flat): 100                 │  71        │  1066      │  1502       │  1503         │
      │ dyn-exp (stateless): 100            │  90        │  5265      │  7006       │  7007         │
      │ dyn-exp(state): 100                 │  96        │  8167      │ 10506       │ 10507         │
      │ dyn cells (stateless)(flat): 100000 │  87        │  1102      │  1542       │  1543         │
      │ dyn cells (state)(flat): 100000     │ 103        │ 12463      │ 15752       │ 15753         │
      │ dyn cols(flat): 100000              │  71        │  1074      │  1514       │  1515         │
      │ dyn-exp (stateless): 100000         │  90        │  5315      │  7073       │  7074         │
      │ dyn-exp(state): 100000              │  96        │  8246      │ 10608       │ 10609         │
      └─────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘

      ======= Startup Incr Annotated Node Counts =======
      ┌────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬─────────────────────┐
      │                            │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_act │
      │                            │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ion_pair            │
      ├────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼─────────────────────┤
      │ dyn cells                  │ 9     │  422  │   762  │ 22        │ 103       │   23  │ 100       │   3       │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                   │
      │ (stateless)(flat): 100     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
      │ dyn cells (state)(flat):   │ 9     │ 4622  │  9862  │ 22        │ 803       │ 1430  │ 100       │   3       │ 800       │ 800       │ 8         │ 8         │ 1         │ 1    │ 0                   │
      │ 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
      │ dyn cols(flat): 100        │ 8     │  403  │   724  │ 22        │ 103       │   23  │ 100       │   3       │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                   │
      │ dyn-exp (stateless): 100   │ 8     │ 3402  │  6522  │ 22        │ 303       │  124  │ 100       │ 103       │ 200       │ 200       │ 2         │ 2         │ 1         │ 1    │ 0                   │
      │ dyn-exp(state): 100        │ 8     │ 4802  │ 10722  │ 22        │ 303       │ 1524  │ 100       │ 103       │ 200       │ 200       │ 2         │ 2         │ 1         │ 1    │ 0                   │
      │ dyn cells                  │ 9     │  425  │   767  │ 22        │ 104       │   23  │ 101       │   3       │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                   │
      │ (stateless)(flat): 100000  │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
      │ dyn cells (state)(flat):   │ 9     │ 4667  │  9958  │ 22        │ 811       │ 1444  │ 101       │   3       │ 808       │ 808       │ 8         │ 8         │ 1         │ 1    │ 0                   │
      │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
      │ dyn cols(flat): 100000     │ 8     │  406  │   729  │ 22        │ 104       │   23  │ 101       │   3       │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                   │
      │ dyn-exp (stateless):       │ 8     │ 3435  │  6585  │ 22        │ 306       │  125  │ 101       │ 104       │ 202       │ 202       │ 2         │ 2         │ 1         │ 1    │ 0                   │
      │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
      │ dyn-exp(state): 100000     │ 8     │ 4849  │ 10827  │ 22        │ 306       │ 1539  │ 101       │ 104       │ 202       │ 202       │ 2         │ 2         │ 1         │ 1    │ 0                   │
      └────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴─────────────────────┘

      ======= Bonsai Computation Nodes =======
      ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
      │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ monito │
      │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ r_free │
      │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ _varia │
      │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ bles   │
      ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
      │ dyn cells           │ 107    │ 0      │ 1     │  9    │ 9      │ 0      │ 134 │ 0     │ 1     │ 1     │ 0      │ 7      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless)(flat):  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ 100                 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cells           │ 149    │ 0      │ 1     │ 16    │ 9      │ 0      │ 176 │ 0     │ 1     │ 1     │ 7      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (state)(flat): 100  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cols(flat): 100 │ 101    │ 0      │ 1     │  9    │ 3      │ 0      │ 115 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ dyn-exp             │ 130    │ 0      │ 1     │  9    │ 2      │ 0      │ 142 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless): 100    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn-exp(state): 100 │ 144    │ 0      │ 1     │ 16    │ 2      │ 0      │ 163 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ dyn cells           │ 107    │ 0      │ 1     │  9    │ 9      │ 0      │ 134 │ 0     │ 1     │ 1     │ 0      │ 7      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless)(flat):  │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cells           │ 149    │ 0      │ 1     │ 16    │ 9      │ 0      │ 176 │ 0     │ 1     │ 1     │ 7      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (state)(flat):      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cols(flat):     │ 101    │ 0      │ 1     │  9    │ 3      │ 0      │ 115 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn-exp             │ 130    │ 0      │ 1     │  9    │ 2      │ 0      │ 142 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless): 100000 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn-exp(state):     │ 144    │ 0      │ 1     │ 16    │ 2      │ 0      │ 163 │ 0     │ 1     │ 1     │ 1      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

      ======= Bonsai Value Nodes =======
      ┌─────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
      │                                     │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
      ├─────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
      │ dyn cells (stateless)(flat): 100    │ 7        │ 0          │ 3    │ 211   │ 21     │ 113  │
      │ dyn cells (state)(flat): 100        │ 7        │ 0          │ 3    │ 267   │ 21     │ 155  │
      │ dyn cols(flat): 100                 │ 7        │ 0          │ 3    │ 186   │ 21     │ 107  │
      │ dyn-exp (stateless): 100            │ 7        │ 1          │ 3    │ 222   │ 21     │ 135  │
      │ dyn-exp(state): 100                 │ 7        │ 1          │ 3    │ 243   │ 21     │ 149  │
      │ dyn cells (stateless)(flat): 100000 │ 7        │ 0          │ 3    │ 211   │ 21     │ 113  │
      │ dyn cells (state)(flat): 100000     │ 7        │ 0          │ 3    │ 267   │ 21     │ 155  │
      │ dyn cols(flat): 100000              │ 7        │ 0          │ 3    │ 186   │ 21     │ 107  │
      │ dyn-exp (stateless): 100000         │ 7        │ 1          │ 3    │ 222   │ 21     │ 135  │
      │ dyn-exp(state): 100000              │ 7        │ 1          │ 3    │ 243   │ 21     │ 149  │
      └─────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
      |}]
  ;;

  let%expect_test "Grouped" =
    test_startup Config.all_grouped;
    [%expect
      {|
      ======= Startup Incr Node Stats =======
      ┌────────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┐
      │                                        │ max_height │ node_count │ max_node_id │ nodes_created │
      ├────────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┤
      │ dyn cells (stateless) (groups): 100    │ 75         │ 1074       │ 1510        │ 1511          │
      │ dyn cells (state) (groups): 100        │ 91         │ 4288       │ 5530        │ 5531          │
      │ dyn cols (groups): 100                 │ 71         │ 1066       │ 1502        │ 1503          │
      │ dyn cells (stateless) (groups): 100000 │ 75         │ 1082       │ 1522        │ 1523          │
      │ dyn cells (state) (groups): 100000     │ 91         │ 4328       │ 5582        │ 5583          │
      │ dyn cols (groups): 100000              │ 71         │ 1074       │ 1514        │ 1515          │
      └────────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┘

      ======= Startup Incr Annotated Node Counts =======
      ┌──────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬───────────────────┐
      │                              │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_a │
      │                              │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ction_pair        │
      ├──────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼───────────────────┤
      │ dyn cells (stateless)        │ 9     │  410  │  738   │ 22        │ 103       │  23   │ 100       │ 3         │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                 │
      │ (groups): 100                │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
      │ dyn cells (state) (groups):  │ 9     │ 1610  │ 3338   │ 22        │ 303       │ 425   │ 100       │ 3         │ 300       │ 300       │ 3         │ 3         │ 1         │ 1    │ 0                 │
      │ 100                          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
      │ dyn cols (groups): 100       │ 8     │  403  │  724   │ 22        │ 103       │  23   │ 100       │ 3         │ 100       │ 100       │ 1         │ 1         │ 1         │ 1    │ 0                 │
      │ dyn cells (stateless)        │ 9     │  413  │  743   │ 22        │ 104       │  23   │ 101       │ 3         │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                 │
      │ (groups): 100000             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
      │ dyn cells (state) (groups):  │ 9     │ 1625  │ 3369   │ 22        │ 306       │ 429   │ 101       │ 3         │ 303       │ 303       │ 3         │ 3         │ 1         │ 1    │ 0                 │
      │ 100000                       │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                   │
      │ dyn cols (groups): 100000    │ 8     │  406  │  729   │ 22        │ 104       │  23   │ 101       │ 3         │ 101       │ 101       │ 1         │ 1         │ 1         │ 1    │ 0                 │
      └──────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴───────────────────┘

      ======= Bonsai Computation Nodes =======
      ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
      │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ monito │
      │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ r_free │
      │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ _varia │
      │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ bles   │
      ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
      │ dyn cells           │ 103    │ 0      │ 1     │  9    │ 6      │ 0      │ 122 │ 0     │ 1     │ 1     │ 0      │ 2      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless)         │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cells (state)   │ 115    │ 0      │ 1     │ 11    │ 6      │ 0      │ 134 │ 0     │ 1     │ 1     │ 2      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cols (groups):  │ 101    │ 0      │ 1     │  9    │ 3      │ 0      │ 115 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ 100                 │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cells           │ 103    │ 0      │ 1     │  9    │ 6      │ 0      │ 122 │ 0     │ 1     │ 1     │ 0      │ 2      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (stateless)         │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cells (state)   │ 115    │ 0      │ 1     │ 11    │ 6      │ 0      │ 134 │ 0     │ 1     │ 1     │ 2      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      │ dyn cols (groups):  │ 101    │ 0      │ 1     │  9    │ 3      │ 0      │ 115 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
      │ 100000              │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
      └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

      ======= Bonsai Value Nodes =======
      ┌────────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
      │                                        │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
      ├────────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
      │ dyn cells (stateless) (groups): 100    │ 7        │ 0          │ 3    │ 195   │ 21     │ 109  │
      │ dyn cells (state) (groups): 100        │ 7        │ 0          │ 3    │ 211   │ 21     │ 121  │
      │ dyn cols (groups): 100                 │ 7        │ 0          │ 3    │ 186   │ 21     │ 107  │
      │ dyn cells (stateless) (groups): 100000 │ 7        │ 0          │ 3    │ 195   │ 21     │ 109  │
      │ dyn cells (state) (groups): 100000     │ 7        │ 0          │ 3    │ 211   │ 21     │ 121  │
      │ dyn cols (groups): 100000              │ 7        │ 0          │ 3    │ 186   │ 21     │ 107  │
      └────────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
      |}]
  ;;
end

let test_interaction configs =
  Report.Interaction.run_and_print_compare (module Config) scenarios configs
;;

module%test [@name "interactions"] _ = struct
  let%expect_test "Flat" =
    test_interaction Config.all_flat;
    [%expect
      {|
      ======= Max Height =======
      ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
      │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
      ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key present) and unfocus in 10 element map                │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key present) and unfocus in 100 element map               │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key present) and unfocus in 101 element map               │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus up and down in 10 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus up and down in 100 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus up and down in 101 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus up and down in 1000 element map                                   │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus up and down in 10000 element map                                  │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus left and right in a map with 10 rows                              │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus left and right in a map with 100 rows                             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus left and right in a map with 101 rows                             │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus left and right in a map with 1000 rows                            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Focus left and right in a map with 10000 rows                           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Page up and down in 10 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Page up and down in 100 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Page up and down in 101 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Page up and down in 1000 element map                                    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Page up and down in 10000 element map                                   │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 100 element map using 10 window          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 101 element map using 10 window          │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Invert ordering of 10 element map                                       │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Invert ordering of 100 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Invert ordering of 101 element map                                      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Invert ordering of 1000 element map                                     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 87                          │ 103                     │ 71             │ 90                  │ 96             │
      └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

      ======= Node Count =======
      ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
      │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
      ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus by key (key present) and unfocus in 10 element map                │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Focus by key (key present) and unfocus in 100 element map               │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      │ Focus by key (key present) and unfocus in 101 element map               │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus up and down in 10 element map                                     │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Focus up and down in 100 element map                                    │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      │ Focus up and down in 101 element map                                    │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus up and down in 1000 element map                                   │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus up and down in 10000 element map                                  │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus left and right in a map with 10 rows                              │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Focus left and right in a map with 100 rows                             │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      │ Focus left and right in a map with 101 rows                             │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus left and right in a map with 1000 rows                            │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Focus left and right in a map with 10000 rows                           │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Page up and down in 10 element map                                      │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Page up and down in 100 element map                                     │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      │ Page up and down in 101 element map                                     │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Page up and down in 1000 element map                                    │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Page up and down in 10000 element map                                   │ 1102                        │ 12463                   │ 1074           │ 5315                │ 8246           │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  306                        │   467                   │  278           │  319                │  350           │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │  378                        │  1547                   │  350           │  769                │ 1061           │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  306                        │   467                   │  278           │  319                │  350           │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │  378                        │  1547                   │  350           │  769                │ 1061           │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1098                        │ 12347                   │ 1070           │ 5269                │ 8171           │
      │ Apply 4 filters and clear with 100 element map using 10 window          │  375                        │  1544                   │  347           │  766                │ 1058           │
      │ Apply 4 filters and clear with 101 element map using 10 window          │  375                        │  1544                   │  347           │  766                │ 1058           │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │  375                        │  1544                   │  347           │  766                │ 1058           │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │  695                        │  6344                   │  667           │ 2766                │ 4218           │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │  695                        │  6344                   │  667           │ 2766                │ 4218           │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1095                        │ 12344                   │ 1067           │ 5266                │ 8168           │
      │ Invert ordering of 10 element map                                       │  375                        │  1544                   │  347           │  766                │ 1058           │
      │ Invert ordering of 100 element map                                      │ 1095                        │ 12344                   │ 1067           │ 5266                │ 8168           │
      │ Invert ordering of 101 element map                                      │ 1103                        │ 12464                   │ 1075           │ 5316                │ 8247           │
      │ Invert ordering of 1000 element map                                     │ 1103                        │ 12464                   │ 1075           │ 5316                │ 8247           │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  374                        │  1543                   │  346           │  765                │ 1057           │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1094                        │ 12343                   │ 1066           │ 5265                │ 8167           │
      └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

      ======= Max Node ID =======
      ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
      │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
      ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus by key (key present) and unfocus in 10 element map                │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Focus by key (key present) and unfocus in 100 element map               │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      │ Focus by key (key present) and unfocus in 101 element map               │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus up and down in 10 element map                                     │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Focus up and down in 100 element map                                    │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      │ Focus up and down in 101 element map                                    │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus up and down in 1000 element map                                   │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus up and down in 10000 element map                                  │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus left and right in a map with 10 rows                              │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Focus left and right in a map with 100 rows                             │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      │ Focus left and right in a map with 101 rows                             │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus left and right in a map with 1000 rows                            │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Focus left and right in a map with 10000 rows                           │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Page up and down in 10 element map                                      │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Page up and down in 100 element map                                     │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      │ Page up and down in 101 element map                                     │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Page up and down in 1000 element map                                    │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Page up and down in 10000 element map                                   │ 1542                        │ 15752                   │ 1514           │  7073               │ 10608          │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 1549                        │ 16963                   │ 1521           │  7777               │ 11837          │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 1739                        │ 18049                   │ 1711           │  8095               │ 12155          │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 1561                        │ 17115                   │ 1533           │  7844               │ 11939          │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 1751                        │ 18201                   │ 1723           │  8162               │ 12257          │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1751                        │ 18201                   │ 1723           │  8162               │ 12257          │
      │ Apply 4 filters and clear with 100 element map using 10 window          │  640                        │  5134                   │  612           │  2858               │  4468          │
      │ Apply 4 filters and clear with 101 element map using 10 window          │  640                        │  5134                   │  612           │  2858               │  4468          │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │  640                        │  5134                   │  612           │  2858               │  4468          │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 1120                        │ 24654                   │ 1092           │ 13058               │ 21668          │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 1120                        │ 24654                   │ 1092           │ 13058               │ 21668          │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1720                        │ 49054                   │ 1692           │ 25808               │ 43168          │
      │ Invert ordering of 10 element map                                       │  501                        │  1971                   │  473           │  1027               │  1377          │
      │ Invert ordering of 100 element map                                      │ 1581                        │ 15651                   │ 1553           │  7057               │ 10557          │
      │ Invert ordering of 101 element map                                      │ 1593                        │ 15803                   │ 1565           │  7124               │ 10659          │
      │ Invert ordering of 1000 element map                                     │ 1593                        │ 15803                   │ 1565           │  7124               │ 10659          │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  450                        │  1920                   │  422           │   976               │  1326          │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1530                        │ 15600                   │ 1502           │  7006               │ 10506          │
      └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘

      ======= Nodes Created =======
      ┌─────────────────────────────────────────────────────────────────────────┬─────────────────────────────┬─────────────────────────┬────────────────┬─────────────────────┬────────────────┐
      │                                                                         │ dyn cells (stateless)(flat) │ dyn cells (state)(flat) │ dyn cols(flat) │ dyn-exp (stateless) │ dyn-exp(state) │
      ├─────────────────────────────────────────────────────────────────────────┼─────────────────────────────┼─────────────────────────┼────────────────┼─────────────────────┼────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key not present) and unfocus in 100 element map           │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key not present) and unfocus in 101 element map           │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key not present) and unfocus in 1000 element map          │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key not present) and unfocus in 10000 element map         │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key present) and unfocus in 10 element map                │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key present) and unfocus in 100 element map               │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key present) and unfocus in 101 element map               │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key present) and unfocus in 1000 element map              │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus by key (key present) and unfocus in 10000 element map             │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus up and down in 10 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus up and down in 100 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus up and down in 101 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus up and down in 1000 element map                                   │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus up and down in 10000 element map                                  │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus left and right in a map with 10 rows                              │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus left and right in a map with 100 rows                             │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus left and right in a map with 101 rows                             │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus left and right in a map with 1000 rows                            │   0                         │     0                   │   0            │     0               │     0          │
      │ Focus left and right in a map with 10000 rows                           │   0                         │     0                   │   0            │     0               │     0          │
      │ Page up and down in 10 element map                                      │   0                         │     0                   │   0            │     0               │     0          │
      │ Page up and down in 100 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
      │ Page up and down in 101 element map                                     │   0                         │     0                   │   0            │     0               │     0          │
      │ Page up and down in 1000 element map                                    │   0                         │     0                   │   0            │     0               │     0          │
      │ Page up and down in 10000 element map                                   │   0                         │     0                   │   0            │     0               │     0          │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  17                         │  1361                   │  17            │   769               │  1329          │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 209                         │  2449                   │ 209            │  1089               │  1649          │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  17                         │  1361                   │  17            │   769               │  1329          │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 209                         │  2449                   │ 209            │  1089               │  1649          │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 209                         │  2449                   │ 209            │  1089               │  1649          │
      │ Apply 4 filters and clear with 100 element map using 10 window          │ 188                         │  3212                   │ 188            │  1880               │  3140          │
      │ Apply 4 filters and clear with 101 element map using 10 window          │ 188                         │  3212                   │ 188            │  1880               │  3140          │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │ 188                         │  3212                   │ 188            │  1880               │  3140          │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 188                         │ 16652                   │ 188            │  9400               │ 16260          │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 188                         │ 16652                   │ 188            │  9400               │ 16260          │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 188                         │ 33452                   │ 188            │ 18800               │ 32660          │
      │ Invert ordering of 10 element map                                       │  49                         │    49                   │  49            │    49               │    49          │
      │ Invert ordering of 100 element map                                      │  49                         │    49                   │  49            │    49               │    49          │
      │ Invert ordering of 101 element map                                      │  49                         │    49                   │  49            │    49               │    49          │
      │ Invert ordering of 1000 element map                                     │  49                         │    49                   │  49            │    49               │    49          │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │   0                         │     0                   │   0            │     0               │     0          │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │   0                         │     0                   │   0            │     0               │     0          │
      └─────────────────────────────────────────────────────────────────────────┴─────────────────────────────┴─────────────────────────┴────────────────┴─────────────────────┴────────────────┘
      |}]
  ;;

  let%expect_test "Grouped" =
    test_interaction Config.all_grouped;
    [%expect
      {|
      ======= Max Height =======
      ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
      │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
      ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │ 75                             │ 91                         │ 71                │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 75                             │ 91                         │ 71                │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 75                             │ 91                         │ 71                │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 75                             │ 91                         │ 71                │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 75                             │ 91                         │ 71                │
      │ Focus by key (key present) and unfocus in 10 element map                │ 75                             │ 91                         │ 71                │
      │ Focus by key (key present) and unfocus in 100 element map               │ 75                             │ 91                         │ 71                │
      │ Focus by key (key present) and unfocus in 101 element map               │ 75                             │ 91                         │ 71                │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 75                             │ 91                         │ 71                │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 75                             │ 91                         │ 71                │
      │ Focus up and down in 10 element map                                     │ 75                             │ 91                         │ 71                │
      │ Focus up and down in 100 element map                                    │ 75                             │ 91                         │ 71                │
      │ Focus up and down in 101 element map                                    │ 75                             │ 91                         │ 71                │
      │ Focus up and down in 1000 element map                                   │ 75                             │ 91                         │ 71                │
      │ Focus up and down in 10000 element map                                  │ 75                             │ 91                         │ 71                │
      │ Focus left and right in a map with 10 rows                              │ 75                             │ 91                         │ 71                │
      │ Focus left and right in a map with 100 rows                             │ 75                             │ 91                         │ 71                │
      │ Focus left and right in a map with 101 rows                             │ 75                             │ 91                         │ 71                │
      │ Focus left and right in a map with 1000 rows                            │ 75                             │ 91                         │ 71                │
      │ Focus left and right in a map with 10000 rows                           │ 75                             │ 91                         │ 71                │
      │ Page up and down in 10 element map                                      │ 75                             │ 91                         │ 71                │
      │ Page up and down in 100 element map                                     │ 75                             │ 91                         │ 71                │
      │ Page up and down in 101 element map                                     │ 75                             │ 91                         │ 71                │
      │ Page up and down in 1000 element map                                    │ 75                             │ 91                         │ 71                │
      │ Page up and down in 10000 element map                                   │ 75                             │ 91                         │ 71                │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 75                             │ 91                         │ 71                │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 75                             │ 91                         │ 71                │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 75                             │ 91                         │ 71                │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 75                             │ 91                         │ 71                │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 100 element map using 10 window          │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 101 element map using 10 window          │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 75                             │ 91                         │ 71                │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 75                             │ 91                         │ 71                │
      │ Invert ordering of 10 element map                                       │ 75                             │ 91                         │ 71                │
      │ Invert ordering of 100 element map                                      │ 75                             │ 91                         │ 71                │
      │ Invert ordering of 101 element map                                      │ 75                             │ 91                         │ 71                │
      │ Invert ordering of 1000 element map                                     │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │ 75                             │ 91                         │ 71                │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 75                             │ 91                         │ 71                │
      └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

      ======= Node Count =======
      ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
      │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
      ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │  354                           │  688                       │  346              │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 1074                           │ 4288                       │ 1066              │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 1082                           │ 4328                       │ 1074              │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 1082                           │ 4328                       │ 1074              │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 1082                           │ 4328                       │ 1074              │
      │ Focus by key (key present) and unfocus in 10 element map                │  354                           │  688                       │  346              │
      │ Focus by key (key present) and unfocus in 100 element map               │ 1074                           │ 4288                       │ 1066              │
      │ Focus by key (key present) and unfocus in 101 element map               │ 1082                           │ 4328                       │ 1074              │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 1082                           │ 4328                       │ 1074              │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 1082                           │ 4328                       │ 1074              │
      │ Focus up and down in 10 element map                                     │  354                           │  688                       │  346              │
      │ Focus up and down in 100 element map                                    │ 1074                           │ 4288                       │ 1066              │
      │ Focus up and down in 101 element map                                    │ 1082                           │ 4328                       │ 1074              │
      │ Focus up and down in 1000 element map                                   │ 1082                           │ 4328                       │ 1074              │
      │ Focus up and down in 10000 element map                                  │ 1082                           │ 4328                       │ 1074              │
      │ Focus left and right in a map with 10 rows                              │  354                           │  688                       │  346              │
      │ Focus left and right in a map with 100 rows                             │ 1074                           │ 4288                       │ 1066              │
      │ Focus left and right in a map with 101 rows                             │ 1082                           │ 4328                       │ 1074              │
      │ Focus left and right in a map with 1000 rows                            │ 1082                           │ 4328                       │ 1074              │
      │ Focus left and right in a map with 10000 rows                           │ 1082                           │ 4328                       │ 1074              │
      │ Page up and down in 10 element map                                      │  354                           │  688                       │  346              │
      │ Page up and down in 100 element map                                     │ 1074                           │ 4288                       │ 1066              │
      │ Page up and down in 101 element map                                     │ 1082                           │ 4328                       │ 1074              │
      │ Page up and down in 1000 element map                                    │ 1082                           │ 4328                       │ 1074              │
      │ Page up and down in 10000 element map                                   │ 1082                           │ 4328                       │ 1074              │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  286                           │  332                       │  278              │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │  358                           │  692                       │  350              │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  286                           │  332                       │  278              │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │  358                           │  692                       │  350              │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1078                           │ 4292                       │ 1070              │
      │ Apply 4 filters and clear with 100 element map using 10 window          │  355                           │  689                       │  347              │
      │ Apply 4 filters and clear with 101 element map using 10 window          │  355                           │  689                       │  347              │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │  355                           │  689                       │  347              │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │  675                           │ 2289                       │  667              │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │  675                           │ 2289                       │  667              │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1075                           │ 4289                       │ 1067              │
      │ Invert ordering of 10 element map                                       │  355                           │  689                       │  347              │
      │ Invert ordering of 100 element map                                      │ 1075                           │ 4289                       │ 1067              │
      │ Invert ordering of 101 element map                                      │ 1083                           │ 4329                       │ 1075              │
      │ Invert ordering of 1000 element map                                     │ 1083                           │ 4329                       │ 1075              │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  354                           │  688                       │  346              │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  354                           │  688                       │  346              │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  354                           │  688                       │  346              │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  354                           │  688                       │  346              │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  354                           │  688                       │  346              │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  354                           │  688                       │  346              │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  354                           │  688                       │  346              │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  354                           │  688                       │  346              │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1074                           │ 4288                       │ 1066              │
      └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

      ======= Max Node ID =======
      ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
      │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
      ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │  430                           │   850                      │  422              │
      │ Focus by key (key not present) and unfocus in 100 element map           │ 1510                           │  5530                      │ 1502              │
      │ Focus by key (key not present) and unfocus in 101 element map           │ 1522                           │  5582                      │ 1514              │
      │ Focus by key (key not present) and unfocus in 1000 element map          │ 1522                           │  5582                      │ 1514              │
      │ Focus by key (key not present) and unfocus in 10000 element map         │ 1522                           │  5582                      │ 1514              │
      │ Focus by key (key present) and unfocus in 10 element map                │  430                           │   850                      │  422              │
      │ Focus by key (key present) and unfocus in 100 element map               │ 1510                           │  5530                      │ 1502              │
      │ Focus by key (key present) and unfocus in 101 element map               │ 1522                           │  5582                      │ 1514              │
      │ Focus by key (key present) and unfocus in 1000 element map              │ 1522                           │  5582                      │ 1514              │
      │ Focus by key (key present) and unfocus in 10000 element map             │ 1522                           │  5582                      │ 1514              │
      │ Focus up and down in 10 element map                                     │  430                           │   850                      │  422              │
      │ Focus up and down in 100 element map                                    │ 1510                           │  5530                      │ 1502              │
      │ Focus up and down in 101 element map                                    │ 1522                           │  5582                      │ 1514              │
      │ Focus up and down in 1000 element map                                   │ 1522                           │  5582                      │ 1514              │
      │ Focus up and down in 10000 element map                                  │ 1522                           │  5582                      │ 1514              │
      │ Focus left and right in a map with 10 rows                              │  430                           │   850                      │  422              │
      │ Focus left and right in a map with 100 rows                             │ 1510                           │  5530                      │ 1502              │
      │ Focus left and right in a map with 101 rows                             │ 1522                           │  5582                      │ 1514              │
      │ Focus left and right in a map with 1000 rows                            │ 1522                           │  5582                      │ 1514              │
      │ Focus left and right in a map with 10000 rows                           │ 1522                           │  5582                      │ 1514              │
      │ Page up and down in 10 element map                                      │  430                           │   850                      │  422              │
      │ Page up and down in 100 element map                                     │ 1510                           │  5530                      │ 1502              │
      │ Page up and down in 101 element map                                     │ 1522                           │  5582                      │ 1514              │
      │ Page up and down in 1000 element map                                    │ 1522                           │  5582                      │ 1514              │
      │ Page up and down in 10000 element map                                   │ 1522                           │  5582                      │ 1514              │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │ 1529                           │  5933                      │ 1521              │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 1719                           │  6379                      │ 1711              │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │ 1541                           │  5985                      │ 1533              │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 1731                           │  6431                      │ 1723              │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 1731                           │  6431                      │ 1723              │
      │ Apply 4 filters and clear with 100 element map using 10 window          │  620                           │  1904                      │  612              │
      │ Apply 4 filters and clear with 101 element map using 10 window          │  620                           │  1904                      │  612              │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │  620                           │  1904                      │  612              │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 1100                           │  7824                      │ 1092              │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 1100                           │  7824                      │ 1092              │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 1700                           │ 15224                      │ 1692              │
      │ Invert ordering of 10 element map                                       │  481                           │   901                      │  473              │
      │ Invert ordering of 100 element map                                      │ 1561                           │  5581                      │ 1553              │
      │ Invert ordering of 101 element map                                      │ 1573                           │  5633                      │ 1565              │
      │ Invert ordering of 1000 element map                                     │ 1573                           │  5633                      │ 1565              │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │  430                           │   850                      │  422              │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │  430                           │   850                      │  422              │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │  430                           │   850                      │  422              │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │  430                           │   850                      │  422              │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │  430                           │   850                      │  422              │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │  430                           │   850                      │  422              │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │  430                           │   850                      │  422              │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │  430                           │   850                      │  422              │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │ 1510                           │  5530                      │ 1502              │
      └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘

      ======= Nodes Created =======
      ┌─────────────────────────────────────────────────────────────────────────┬────────────────────────────────┬────────────────────────────┬───────────────────┐
      │                                                                         │ dyn cells (stateless) (groups) │ dyn cells (state) (groups) │ dyn cols (groups) │
      ├─────────────────────────────────────────────────────────────────────────┼────────────────────────────────┼────────────────────────────┼───────────────────┤
      │ Focus by key (key not present) and unfocus in 10 element map            │   0                            │    0                       │   0               │
      │ Focus by key (key not present) and unfocus in 100 element map           │   0                            │    0                       │   0               │
      │ Focus by key (key not present) and unfocus in 101 element map           │   0                            │    0                       │   0               │
      │ Focus by key (key not present) and unfocus in 1000 element map          │   0                            │    0                       │   0               │
      │ Focus by key (key not present) and unfocus in 10000 element map         │   0                            │    0                       │   0               │
      │ Focus by key (key present) and unfocus in 10 element map                │   0                            │    0                       │   0               │
      │ Focus by key (key present) and unfocus in 100 element map               │   0                            │    0                       │   0               │
      │ Focus by key (key present) and unfocus in 101 element map               │   0                            │    0                       │   0               │
      │ Focus by key (key present) and unfocus in 1000 element map              │   0                            │    0                       │   0               │
      │ Focus by key (key present) and unfocus in 10000 element map             │   0                            │    0                       │   0               │
      │ Focus up and down in 10 element map                                     │   0                            │    0                       │   0               │
      │ Focus up and down in 100 element map                                    │   0                            │    0                       │   0               │
      │ Focus up and down in 101 element map                                    │   0                            │    0                       │   0               │
      │ Focus up and down in 1000 element map                                   │   0                            │    0                       │   0               │
      │ Focus up and down in 10000 element map                                  │   0                            │    0                       │   0               │
      │ Focus left and right in a map with 10 rows                              │   0                            │    0                       │   0               │
      │ Focus left and right in a map with 100 rows                             │   0                            │    0                       │   0               │
      │ Focus left and right in a map with 101 rows                             │   0                            │    0                       │   0               │
      │ Focus left and right in a map with 1000 rows                            │   0                            │    0                       │   0               │
      │ Focus left and right in a map with 10000 rows                           │   0                            │    0                       │   0               │
      │ Page up and down in 10 element map                                      │   0                            │    0                       │   0               │
      │ Page up and down in 100 element map                                     │   0                            │    0                       │   0               │
      │ Page up and down in 101 element map                                     │   0                            │    0                       │   0               │
      │ Page up and down in 1000 element map                                    │   0                            │    0                       │   0               │
      │ Page up and down in 10000 element map                                   │   0                            │    0                       │   0               │
      │ Scroll 1-wide window from 0 to 9 and back in 100 element map            │  17                            │  401                       │  17               │
      │ Scroll 10-wide window from 0 to 9 and back in 100 element map           │ 209                            │  849                       │ 209               │
      │ Scroll 1-wide window from 0 to 9 and back in 1000 element map           │  17                            │  401                       │  17               │
      │ Scroll 10-wide window from 0 to 9 and back in 1000 element map          │ 209                            │  849                       │ 209               │
      │ Scroll 100-wide window from 0 to 9 and back in 1000 element map         │ 209                            │  849                       │ 209               │
      │ Apply 4 filters and clear with 100 element map using 10 window          │ 188                            │ 1052                       │ 188               │
      │ Apply 4 filters and clear with 101 element map using 10 window          │ 188                            │ 1052                       │ 188               │
      │ Apply 4 filters and clear with 1000 element map using 10 window         │ 188                            │ 1052                       │ 188               │
      │ Apply 4 filters and clear with 1000 element map using 50 window         │ 188                            │ 4892                       │ 188               │
      │ Apply 4 filters and clear with 10000 element map using 50 window        │ 188                            │ 4892                       │ 188               │
      │ Apply 4 filters and clear with 10000 element map using 100 window       │ 188                            │ 9692                       │ 188               │
      │ Invert ordering of 10 element map                                       │  49                            │   49                       │  49               │
      │ Invert ordering of 100 element map                                      │  49                            │   49                       │  49               │
      │ Invert ordering of 101 element map                                      │  49                            │   49                       │  49               │
      │ Invert ordering of 1000 element map                                     │  49                            │   49                       │  49               │
      │ Perform 10 sets of 1 items in a 10 element map with 10-wide window      │   0                            │    0                       │   0               │
      │ Perform 10 sets of 5 items in a 10 element map with 10-wide window      │   0                            │    0                       │   0               │
      │ Perform 10 sets of 1 items in a 11 element map with 10-wide window      │   0                            │    0                       │   0               │
      │ Perform 10 sets of 5 items in a 11 element map with 10-wide window      │   0                            │    0                       │   0               │
      │ Perform 10 sets of 1 items in a 100 element map with 10-wide window     │   0                            │    0                       │   0               │
      │ Perform 10 sets of 5 items in a 100 element map with 10-wide window     │   0                            │    0                       │   0               │
      │ Perform 10 sets of 1 items in a 1000 element map with 10-wide window    │   0                            │    0                       │   0               │
      │ Perform 10 sets of 5 items in a 1000 element map with 10-wide window    │   0                            │    0                       │   0               │
      │ Perform 10 sets of 10 items in a 1000 element map with 100-wide window  │   0                            │    0                       │   0               │
      └─────────────────────────────────────────────────────────────────────────┴────────────────────────────────┴────────────────────────────┴───────────────────┘
      |}]
  ;;
end
