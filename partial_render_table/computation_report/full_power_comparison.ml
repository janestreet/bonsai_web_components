open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Report = Bonsai_web_test.Computation_report

(* This test compares the "most powerful" version of each API.

   These are the tests where we want to focus most on improving our numbers. *)

let%expect_test "" =
  Report.Startup.run_and_print_compare
    ~computations:(force Config.full_power_comparison)
    (Symbol_table.startup_inputs [ 100; 100_000 ]);
  [%expect
    {|
    ======= Startup Incr Node Stats =======
    ┌────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┬──────────────────┬───────────────────┐
    │                                    │ max_height │ node_count │ max_node_id │ nodes_created │ nodes_recomputed │ nodes_invalidated │
    ├────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┼──────────────────┼───────────────────┤
    │ new (incr cells) (dynamic): 100    │ 77         │  7465      │ 10205       │ 10205         │  7467            │ 0                 │
    │ new (incr rows) (dynamic): 100     │ 73         │  2965      │  3705       │  3705         │  2967            │ 0                 │
    │ new (pure) (dynamic): 100          │ 62         │   974      │  1414       │  1412         │   976            │ 0                 │
    │ new (incr cells) (static): 100     │ 77         │  6966      │  7707       │  7706         │  6968            │ 0                 │
    │ new (incr rows) (static): 100      │ 73         │  3466      │  4207       │  4206         │  3468            │ 0                 │
    │ new (pure) (static): 100           │ 62         │   974      │  1415       │  1412         │   976            │ 0                 │
    │ dyn cols: 100                      │ 63         │   970      │  1407       │  1408         │   972            │ 0                 │
    │ dyn cells: 100                     │ 92         │ 11830      │ 14091       │ 14089         │ 11832            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │ 77         │  7537      │ 10304       │ 10304         │  7539            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │ 73         │  2992      │  3739       │  3739         │  2994            │ 0                 │
    │ new (pure) (dynamic): 100000       │ 62         │   981      │  1425       │  1423         │   983            │ 0                 │
    │ new (incr cells) (static): 100000  │ 77         │  7033      │  7781       │  7780         │  7035            │ 0                 │
    │ new (incr rows) (static): 100000   │ 73         │  3498      │  4246       │  4245         │  3500            │ 0                 │
    │ new (pure) (static): 100000        │ 62         │   981      │  1426       │  1423         │   983            │ 0                 │
    │ dyn cols: 100000                   │ 63         │   977      │  1418       │  1419         │   979            │ 0                 │
    │ dyn cells: 100000                  │ 92         │ 11945      │ 14228       │ 14226         │ 11947            │ 0                 │
    └────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┴──────────────────┴───────────────────┘

    ======= Startup Incr Annotated Node Counts =======
    ┌────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬─────────────────────┐
    │                            │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_act │
    │                            │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ion_pair            │
    ├────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼─────────────────────┤
    │ new (incr cells)           │ 9     │ 3317  │ 7052   │ 23        │ 803       │  724  │ 700       │ 3         │ 800       │ 800       │ 102       │ 102       │ 101       │ 1    │ 0                   │
    │ (dynamic): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows)            │ 9     │ 1317  │ 2652   │ 23        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (dynamic): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (dynamic): 100  │ 9     │  423  │  766   │ 26        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ new (incr cells)           │ 9     │ 3817  │ 8652   │ 23        │ 203       │ 1224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (static): 100              │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows) (static):  │ 9     │ 1817  │ 3652   │ 23        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (static): 100   │ 9     │  422  │  764   │ 26        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cols: 100              │ 9     │  419  │  758   │ 23        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cells: 100             │ 9     │ 4641  │ 9799   │ 23        │ 704       │ 1230  │ 100       │ 4         │ 700       │ 700       │   7       │   7       │   1       │ 1    │ 0                   │
    │ new (incr cells)           │ 9     │ 3349  │ 7120   │ 23        │ 811       │  731  │ 707       │ 3         │ 808       │ 808       │ 103       │ 103       │ 102       │ 1    │ 0                   │
    │ (dynamic): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows)            │ 9     │ 1329  │ 2676   │ 23        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (dynamic): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (dynamic):      │ 9     │  426  │  771   │ 26        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr cells)           │ 9     │ 3854  │ 8736   │ 23        │ 205       │ 1236  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (static): 100000           │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows) (static):  │ 9     │ 1834  │ 3686   │ 23        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (static):       │ 9     │  425  │  769   │ 26        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ dyn cols: 100000           │ 9     │  422  │  763   │ 23        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cells: 100000          │ 9     │ 4686  │ 9894   │ 23        │ 711       │ 1242  │ 101       │ 4         │ 707       │ 707       │   7       │   7       │   1       │ 1    │ 0                   │
    └────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴─────────────────────┘

    ======= Bonsai Computation Nodes =======
    ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
    │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ comput │
    │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ ation_ │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ watche │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ r      │
    ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
    │ new (incr cells)    │ 122    │ 0      │ 1     │ 10    │  2     │ 0      │ 137 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 123    │ 0      │ 1     │ 10    │  2     │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 120    │ 0      │ 1     │ 10    │  3     │ 0      │ 136 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr cells)    │ 149    │ 0      │ 1     │ 15    │  2     │ 0      │ 168 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 129    │ 0      │ 1     │ 10    │  2     │ 0      │ 143 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 120    │ 0      │ 1     │ 10    │  3     │ 0      │ 135 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols: 100       │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 132 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ dyn cells: 100      │ 174    │ 0      │ 1     │ 16    │ 10     │ 0      │ 201 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ new (incr cells)    │ 122    │ 0      │ 1     │ 10    │  2     │ 0      │ 137 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 123    │ 0      │ 1     │ 10    │  2     │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 120    │ 0      │ 1     │ 10    │  3     │ 0      │ 136 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr cells)    │ 149    │ 0      │ 1     │ 15    │  2     │ 0      │ 168 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 129    │ 0      │ 1     │ 10    │  2     │ 0      │ 143 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 120    │ 0      │ 1     │ 10    │  3     │ 0      │ 135 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols: 100000    │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 132 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ dyn cells: 100000   │ 174    │ 0      │ 1     │ 16    │ 10     │ 0      │ 201 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

    ======= Bonsai Value Nodes =======
    ┌────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
    │                                    │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
    ├────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
    │ new (incr cells) (dynamic): 100    │ 3        │ 0          │ 6    │ 233   │ 19     │ 128  │
    │ new (incr rows) (dynamic): 100     │ 3        │ 0          │ 6    │ 233   │ 19     │ 129  │
    │ new (pure) (dynamic): 100          │ 3        │ 0          │ 6    │ 228   │ 19     │ 126  │
    │ new (incr cells) (static): 100     │ 3        │ 0          │ 6    │ 271   │ 19     │ 155  │
    │ new (incr rows) (static): 100      │ 3        │ 0          │ 6    │ 241   │ 19     │ 135  │
    │ new (pure) (static): 100           │ 3        │ 0          │ 6    │ 226   │ 19     │ 126  │
    │ dyn cols: 100                      │ 3        │ 0          │ 4    │ 224   │ 19     │ 123  │
    │ dyn cells: 100                     │ 3        │ 0          │ 8    │ 319   │ 19     │ 180  │
    │ new (incr cells) (dynamic): 100000 │ 3        │ 0          │ 6    │ 233   │ 19     │ 128  │
    │ new (incr rows) (dynamic): 100000  │ 3        │ 0          │ 6    │ 233   │ 19     │ 129  │
    │ new (pure) (dynamic): 100000       │ 3        │ 0          │ 6    │ 228   │ 19     │ 126  │
    │ new (incr cells) (static): 100000  │ 3        │ 0          │ 6    │ 271   │ 19     │ 155  │
    │ new (incr rows) (static): 100000   │ 3        │ 0          │ 6    │ 241   │ 19     │ 135  │
    │ new (pure) (static): 100000        │ 3        │ 0          │ 6    │ 226   │ 19     │ 126  │
    │ dyn cols: 100000                   │ 3        │ 0          │ 4    │ 224   │ 19     │ 123  │
    │ dyn cells: 100000                  │ 3        │ 0          │ 8    │ 319   │ 19     │ 180  │
    └────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
    |}];
  Report.Interaction.run_and_print_compare
    ~get_inject:Config.get_inject
    ~computations:(force Config.full_power_comparison)
    Symbol_table.scenarios;
  [%expect
    {|
    ====== Node Count ======
    ┌───────────────────────────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬──────────┬───────────┐
    │                                           │ new (incr cells)    │ new (incr rows)     │ new (pure)          │ new (incr cells)    │ new (incr rows)     │ new (pure) (static) │ dyn cols │ dyn cells │
    │                                           │ (dynamic)           │ (dynamic)           │ (dynamic)           │ (static)            │ (static)            │                     │          │           │
    ├───────────────────────────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼──────────┼───────────┤
    │ Focus by key (key not present) and        │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ Focus up and down in 100 element map      │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ Focus up and down in 101 element map      │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Focus up and down in 1000 element map     │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Focus up and down in 10000 element map    │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Focus left and right in a map with 10     │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ Page up and down in 100 element map       │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ Page up and down in 101 element map       │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Page up and down in 1000 element map      │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Page up and down in 10000 element map     │ 7537                │ 2992                │ 981                 │ 7033                │ 3498                │ 981                 │ 977      │ 11945     │
    │ Scroll 1-wide window from 0 to 9 and      │  335                │  290                │ 279                 │  331                │  296                │ 279                 │ 275      │   443     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  335                │  290                │ 279                 │  331                │  296                │ 279                 │ 275      │   443     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7463                │ 2963                │ 972                 │ 6964                │ 3464                │ 972                 │ 968      │ 11828     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3864                │ 1614                │ 623                 │ 3615                │ 1865                │ 623                 │ 619      │  6079     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3864                │ 1614                │ 623                 │ 3615                │ 1865                │ 623                 │ 619      │  6079     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7464                │ 2964                │ 973                 │ 6965                │ 3465                │ 973                 │ 969      │ 11829     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ Invert ordering of 100 element map        │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ Invert ordering of 101 element map        │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Invert ordering of 1000 element map       │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Randomly select a row, then change one    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  983                │  533                │ 342                 │  934                │  584                │ 342                 │ 338      │  1478     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7463                │ 2963                │ 972                 │ 6964                │ 3464                │ 972                 │ 968      │ 11828     │
    │ element map with 100-wide window          │                     │                     │                     │                     │                     │                     │          │           │
    └───────────────────────────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴──────────┴───────────┘

    ====== Nodes Created ======
    ┌───────────────────────────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬──────────┬───────────┐
    │                                           │ new (incr cells)    │ new (incr rows)     │ new (pure)          │ new (incr cells)    │ new (incr rows)     │ new (pure) (static) │ dyn cols │ dyn cells │
    │                                           │ (dynamic)           │ (dynamic)           │ (dynamic)           │ (static)            │ (static)            │                     │          │           │
    ├───────────────────────────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼──────────┼───────────┤
    │ Focus by key (key not present) and        │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │    25               │   25                │  25                 │    25               │   25                │  25                 │  25      │    25     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 100 element map      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 101 element map      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 1000 element map     │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 10000 element map    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus left and right in a map with 10     │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 100 element map       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 101 element map       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 1000 element map      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 10000 element map     │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Scroll 1-wide window from 0 to 9 and      │  1307               │  267                │  27                 │   907               │  347                │  27                 │  27      │  1275     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1611               │  571                │ 203                 │  1211               │  651                │ 203                 │ 203      │  2219     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  1307               │  267                │  27                 │   907               │  347                │  27                 │  27      │  1275     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1611               │  571                │ 203                 │  1211               │  651                │ 203                 │ 203      │  2219     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  1611               │  571                │ 203                 │  1211               │  651                │ 203                 │ 203      │  2219     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3035               │  695                │ 155                 │  2135               │  875                │ 155                 │ 155      │  2963     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3035               │  695                │ 155                 │  2135               │  875                │ 155                 │ 155      │  2963     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3035               │  695                │ 155                 │  2135               │  875                │ 155                 │ 155      │  2963     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 15835               │ 3095                │ 155                 │ 10935               │ 4075                │ 155                 │ 155      │ 15443     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 15835               │ 3095                │ 155                 │ 10935               │ 4075                │ 155                 │ 155      │ 15443     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 31835               │ 6095                │ 155                 │ 21935               │ 8075                │ 155                 │ 155      │ 31043     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │    35               │   35                │  35                 │    35               │   35                │  35                 │  35      │    35     │
    │ Invert ordering of 100 element map        │    35               │   35                │  35                 │    35               │   35                │  35                 │  35      │    35     │
    │ Invert ordering of 101 element map        │    35               │   35                │  35                 │    35               │   35                │  35                 │  35      │    35     │
    │ Invert ordering of 1000 element map       │    35               │   35                │  35                 │    35               │   35                │  35                 │  35      │    35     │
    │ Randomly select a row, then change one    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │    23               │   23                │  23                 │    23               │   23                │  23                 │  23      │    23     │
    │ element map with 100-wide window          │                     │                     │                     │                     │                     │                     │          │           │
    └───────────────────────────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴──────────┴───────────┘

    ====== Nodes Recomputed ======
    ┌───────────────────────────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬──────────┬───────────┐
    │                                           │ new (incr cells)    │ new (incr rows)     │ new (pure)          │ new (incr cells)    │ new (incr rows)     │ new (pure) (static) │ dyn cols │ dyn cells │
    │                                           │ (dynamic)           │ (dynamic)           │ (dynamic)           │ (static)            │ (static)            │                     │          │           │
    ├───────────────────────────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼──────────┼───────────┤
    │ Focus by key (key not present) and        │   145               │   144               │  146                │   144               │   144               │  146                │  142     │   146     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   144               │   144               │  146                │   144               │   144               │  146                │  142     │   146     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   144               │   144               │  146                │   144               │   144               │  146                │  142     │   146     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   144               │   144               │  146                │   144               │   144               │  146                │  142     │   146     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   144               │   144               │  146                │   144               │   144               │  146                │  142     │   146     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   220               │   220               │  222                │   220               │   220               │  222                │  218     │   222     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   400               │   400               │  402                │   400               │   400               │  402                │  398     │   402     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   402               │   402               │  404                │   402               │   402               │  404                │  400     │   404     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   402               │   402               │  404                │   402               │   402               │  404                │  400     │   404     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   402               │   402               │  404                │   402               │   402               │  404                │  400     │   404     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │    69               │    69               │   71                │    69               │    69               │   71                │   69     │    69     │
    │ Focus up and down in 100 element map      │   159               │   159               │  161                │   159               │   159               │  161                │  159     │   159     │
    │ Focus up and down in 101 element map      │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Focus up and down in 1000 element map     │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Focus up and down in 10000 element map    │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Focus left and right in a map with 10     │    69               │    69               │   71                │    69               │    69               │   71                │   69     │    69     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │   159               │   159               │  161                │   159               │   159               │  161                │  159     │   159     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │    69               │    69               │   71                │    69               │    69               │   71                │   69     │    69     │
    │ Page up and down in 100 element map       │   159               │   159               │  161                │   159               │   159               │  161                │  159     │   159     │
    │ Page up and down in 101 element map       │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Page up and down in 1000 element map      │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Page up and down in 10000 element map     │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Scroll 1-wide window from 0 to 9 and      │  2340               │  1620               │ 1349                │  2260               │  1700               │ 1349                │ 1349     │  3710     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2356               │  1636               │ 1365                │  2276               │  1716               │ 1365                │ 1365     │  3726     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  2340               │  1620               │ 1349                │  2260               │  1700               │ 1349                │ 1349     │  3710     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2356               │  1636               │ 1365                │  2276               │  1716               │ 1365                │ 1365     │  3726     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  2356               │  1636               │ 1365                │  2276               │  1716               │ 1365                │ 1365     │  3726     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3008               │  1388               │  682                │  2828               │  1568               │  682                │  682     │  4818     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3008               │  1388               │  682                │  2828               │  1568               │  682                │  682     │  4818     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3008               │  1388               │  682                │  2828               │  1568               │  682                │  682     │  4818     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14368               │  5548               │ 1642                │ 13388               │  6528               │ 1642                │ 1642     │ 23058     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14368               │  5548               │ 1642                │ 13388               │  6528               │ 1642                │ 1642     │ 23058     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28568               │ 10748               │ 2842                │ 26588               │ 12728               │ 2842                │ 2842     │ 45858     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   122               │   122               │  141                │   122               │   122               │  141                │  141     │   396     │
    │ Invert ordering of 100 element map        │   482               │   482               │  591                │   482               │   482               │  591                │  591     │  3006     │
    │ Invert ordering of 101 element map        │   486               │   486               │  596                │   486               │   486               │  596                │  596     │  3035     │
    │ Invert ordering of 1000 element map       │   486               │   486               │  596                │   486               │   486               │  596                │  596     │  3035     │
    │ Randomly select a row, then change one    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  1089               │   994               │  813                │  1184               │  1089               │  813                │  813     │  2213     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  1845               │  1570               │  993                │  2120               │  1845               │  993                │  993     │  4337     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  1089               │   994               │  813                │  1184               │  1089               │  813                │  813     │  2213     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  1845               │  1570               │  993                │  2120               │  1845               │  993                │  993     │  4337     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  1089               │   994               │  813                │  1184               │  1089               │  813                │  813     │  2213     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  1845               │  1570               │  993                │  2120               │  1845               │  993                │  993     │  4337     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  1089               │   994               │  813                │  1184               │  1089               │  813                │  813     │  2213     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  1845               │  1570               │  993                │  2120               │  1845               │  993                │  993     │  4337     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │  4680               │  3730               │ 1668                │  5630               │  4680               │ 1668                │ 1668     │ 12302     │
    │ element map with 100-wide window          │                     │                     │                     │                     │                     │                     │          │           │
    └───────────────────────────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴──────────┴───────────┘

    ====== Nodes Invalidated ======
    ┌───────────────────────────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬──────────┬───────────┐
    │                                           │ new (incr cells)    │ new (incr rows)     │ new (pure)          │ new (incr cells)    │ new (incr rows)     │ new (pure) (static) │ dyn cols │ dyn cells │
    │                                           │ (dynamic)           │ (dynamic)           │ (dynamic)           │ (static)            │ (static)            │                     │          │           │
    ├───────────────────────────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼──────────┼───────────┤
    │ Focus by key (key not present) and        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 100 element map      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 101 element map      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 1000 element map     │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus up and down in 10000 element map    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Focus left and right in a map with 10     │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 100 element map       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 101 element map       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 1000 element map      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Page up and down in 10000 element map     │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ Scroll 1-wide window from 0 to 9 and      │  532                │  452                │ 129                 │  1092               │  532                │ 129                 │ 129      │  1875     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  242                │  242                │ 136                 │   242               │  242                │ 136                 │ 136      │   772     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  534                │  454                │ 130                 │  1094               │  534                │ 130                 │ 130      │  1882     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  244                │  244                │ 137                 │   244               │  244                │ 137                 │ 137      │   779     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │   64                │   64                │  47                 │    64               │   64                │  47                 │  47      │   149     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  837                │  657                │ 153                 │  2097               │  837                │ 153                 │ 153      │  2745     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  837                │  657                │ 153                 │  2097               │  837                │ 153                 │ 153      │  2745     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  837                │  657                │ 153                 │  2097               │  837                │ 153                 │ 153      │  2745     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3877                │ 2897                │ 153                 │ 10737               │ 3877                │ 153                 │ 153      │ 14265     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3877                │ 2897                │ 153                 │ 10737               │ 3877                │ 153                 │ 153      │ 14265     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7677                │ 5697                │ 153                 │ 21537               │ 7677                │ 153                 │ 153      │ 28665     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   33                │   33                │  33                 │    33               │   33                │  33                 │  33      │    33     │
    │ Invert ordering of 100 element map        │   33                │   33                │  33                 │    33               │   33                │  33                 │  33      │    33     │
    │ Invert ordering of 101 element map        │   33                │   33                │  33                 │    33               │   33                │  33                 │  33      │    33     │
    │ Invert ordering of 1000 element map       │   33                │   33                │  33                 │    33               │   33                │  33                 │  33      │    33     │
    │ Randomly select a row, then change one    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │   26                │   26                │  26                 │    26               │   26                │  26                 │  26      │    26     │
    │ element map with 100-wide window          │                     │                     │                     │                     │                     │                     │          │           │
    └───────────────────────────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴──────────┴───────────┘
    |}]
;;
