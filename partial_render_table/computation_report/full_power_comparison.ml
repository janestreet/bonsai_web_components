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
    │ new (incr cells) (dynamic): 100    │ 78         │  7364      │ 10104       │ 10104         │  7366            │ 0                 │
    │ new (incr rows) (dynamic): 100     │ 74         │  2864      │  3604       │  3604         │  2866            │ 0                 │
    │ new (pure) (dynamic): 100          │ 63         │   974      │  1414       │  1412         │   976            │ 0                 │
    │ new (incr cells) (static): 100     │ 78         │  6865      │  7606       │  7605         │  6867            │ 0                 │
    │ new (incr rows) (static): 100      │ 74         │  3365      │  4106       │  4105         │  3367            │ 0                 │
    │ new (pure) (static): 100           │ 63         │   974      │  1415       │  1412         │   976            │ 0                 │
    │ dyn cols: 100                      │ 64         │   970      │  1407       │  1408         │   972            │ 0                 │
    │ dyn cells: 100                     │ 93         │ 11224      │ 13485       │ 13483         │ 11226            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │ 78         │  7435      │ 10202       │ 10202         │  7437            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │ 74         │  2890      │  3637       │  3637         │  2892            │ 0                 │
    │ new (pure) (dynamic): 100000       │ 63         │   981      │  1425       │  1423         │   983            │ 0                 │
    │ new (incr cells) (static): 100000  │ 78         │  6931      │  7679       │  7678         │  6933            │ 0                 │
    │ new (incr rows) (static): 100000   │ 74         │  3396      │  4144       │  4143         │  3398            │ 0                 │
    │ new (pure) (static): 100000        │ 63         │   981      │  1426       │  1423         │   983            │ 0                 │
    │ dyn cols: 100000                   │ 64         │   977      │  1418       │  1419         │   979            │ 0                 │
    │ dyn cells: 100000                  │ 93         │ 11333      │ 13616       │ 13614         │ 11335            │ 0                 │
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
    │ Focus by key (key not present) and        │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7364                │ 2864                │ 974                 │ 6865                │ 3365                │ 974                 │ 970      │ 11224     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7364                │ 2864                │ 974                 │ 6865                │ 3365                │ 974                 │ 970      │ 11224     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ Focus up and down in 100 element map      │ 7364                │ 2864                │ 974                 │ 6865                │ 3365                │ 974                 │ 970      │ 11224     │
    │ Focus up and down in 101 element map      │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Focus up and down in 1000 element map     │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Focus up and down in 10000 element map    │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Focus left and right in a map with 10     │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7364                │ 2864                │ 974                 │ 6865                │ 3365                │ 974                 │ 970      │ 11224     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ Page up and down in 100 element map       │ 7364                │ 2864                │ 974                 │ 6865                │ 3365                │ 974                 │ 970      │ 11224     │
    │ Page up and down in 101 element map       │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Page up and down in 1000 element map      │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Page up and down in 10000 element map     │ 7435                │ 2890                │ 981                 │ 6931                │ 3396                │ 981                 │ 977      │ 11333     │
    │ Scroll 1-wide window from 0 to 9 and      │  334                │  289                │ 280                 │  330                │  295                │ 280                 │ 276      │   432     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  973                │  523                │ 343                 │  924                │  574                │ 343                 │ 339      │  1413     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  334                │  289                │ 280                 │  330                │  295                │ 280                 │ 276      │   432     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  973                │  523                │ 343                 │  924                │  574                │ 343                 │ 339      │  1413     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7363                │ 2863                │ 973                 │ 6864                │ 3364                │ 973                 │ 969      │ 11223     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  973                │  523                │ 343                 │  924                │  574                │ 343                 │ 339      │  1413     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  973                │  523                │ 343                 │  924                │  574                │ 343                 │ 339      │  1413     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  973                │  523                │ 343                 │  924                │  574                │ 343                 │ 339      │  1413     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3813                │ 1563                │ 623                 │ 3564                │ 1814                │ 623                 │ 619      │  5773     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3813                │ 1563                │ 623                 │ 3564                │ 1814                │ 623                 │ 619      │  5773     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7363                │ 2863                │ 973                 │ 6864                │ 3364                │ 973                 │ 969      │ 11223     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  975                │  525                │ 345                 │  926                │  576                │ 345                 │ 341      │  1415     │
    │ Invert ordering of 100 element map        │ 7365                │ 2865                │ 975                 │ 6866                │ 3366                │ 975                 │ 971      │ 11225     │
    │ Invert ordering of 101 element map        │ 7436                │ 2891                │ 982                 │ 6932                │ 3397                │ 982                 │ 978      │ 11334     │
    │ Invert ordering of 1000 element map       │ 7436                │ 2891                │ 982                 │ 6932                │ 3397                │ 982                 │ 978      │ 11334     │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  974                │  524                │ 344                 │  925                │  575                │ 344                 │ 340      │  1414     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  972                │  522                │ 342                 │  923                │  573                │ 342                 │ 338      │  1412     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7362                │ 2862                │ 972                 │ 6863                │ 3363                │ 972                 │ 968      │ 11222     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  1292               │  252                │  28                 │   892               │  332                │  28                 │  28      │  1180     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1596               │  556                │ 204                 │  1196               │  636                │ 204                 │ 204      │  2124     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  1292               │  252                │  28                 │   892               │  332                │  28                 │  28      │  1180     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1596               │  556                │ 204                 │  1196               │  636                │ 204                 │ 204      │  2124     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  1596               │  556                │ 204                 │  1196               │  636                │ 204                 │ 204      │  2124     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  2995               │  655                │ 151                 │  2095               │  835                │ 151                 │ 151      │  2743     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  2995               │  655                │ 151                 │  2095               │  835                │ 151                 │ 151      │  2743     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  2995               │  655                │ 151                 │  2095               │  835                │ 151                 │ 151      │  2743     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 15635               │ 2895                │ 151                 │ 10735               │ 3875                │ 151                 │ 151      │ 14263     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 15635               │ 2895                │ 151                 │ 10735               │ 3875                │ 151                 │ 151      │ 14263     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 31435               │ 5695                │ 151                 │ 21535               │ 7675                │ 151                 │ 151      │ 28663     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │    34               │   34                │  34                 │    34               │   34                │  34                 │  34      │    34     │
    │ Invert ordering of 100 element map        │    34               │   34                │  34                 │    34               │   34                │  34                 │  34      │    34     │
    │ Invert ordering of 101 element map        │    34               │   34                │  34                 │    34               │   34                │  34                 │  34      │    34     │
    │ Invert ordering of 1000 element map       │    34               │   34                │  34                 │    34               │   34                │  34                 │  34      │    34     │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
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
    │ Scroll 1-wide window from 0 to 9 and      │  2341               │  1621               │ 1366                │  2261               │  1701               │ 1366                │ 1366     │  3631     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2357               │  1637               │ 1382                │  2277               │  1717               │ 1382                │ 1382     │  3647     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  2341               │  1621               │ 1366                │  2261               │  1701               │ 1366                │ 1366     │  3631     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2357               │  1637               │ 1382                │  2277               │  1717               │ 1382                │ 1382     │  3647     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  2357               │  1637               │ 1382                │  2277               │  1717               │ 1382                │ 1382     │  3647     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  2972               │  1352               │  682                │  2792               │  1532               │  682                │  682     │  4602     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  2972               │  1352               │  682                │  2792               │  1532               │  682                │  682     │  4602     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  2972               │  1352               │  682                │  2792               │  1532               │  682                │  682     │  4602     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14172               │  5352               │ 1642                │ 13192               │  6332               │ 1642                │ 1642     │ 21882     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14172               │  5352               │ 1642                │ 13192               │  6332               │ 1642                │ 1642     │ 21882     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28172               │ 10352               │ 2842                │ 26192               │ 12332               │ 2842                │ 2842     │ 43482     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   122               │   122               │  141                │   122               │   122               │  141                │  141     │   396     │
    │ Invert ordering of 100 element map        │   482               │   482               │  591                │   482               │   482               │  591                │  591     │  3006     │
    │ Invert ordering of 101 element map        │   486               │   486               │  596                │   486               │   486               │  596                │  596     │  3035     │
    │ Invert ordering of 1000 element map       │   486               │   486               │  596                │   486               │   486               │  596                │  596     │  3035     │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │     0               │     0               │    0                │     0               │     0               │    0                │    0     │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  1099               │  1004               │  823                │  1194               │  1099               │  823                │  823     │  2223     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  1855               │  1580               │ 1003                │  2130               │  1855               │ 1003                │ 1003     │  4347     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  1099               │  1004               │  823                │  1194               │  1099               │  823                │  823     │  2223     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  1855               │  1580               │ 1003                │  2130               │  1855               │ 1003                │ 1003     │  4347     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  1099               │  1004               │  823                │  1194               │  1099               │  823                │  823     │  2223     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  1855               │  1580               │ 1003                │  2130               │  1855               │ 1003                │ 1003     │  4347     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  1099               │  1004               │  823                │  1194               │  1099               │  823                │  823     │  2223     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  1855               │  1580               │ 1003                │  2130               │  1855               │ 1003                │ 1003     │  4347     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │  4690               │  3740               │ 1678                │  5640               │  4690               │ 1678                │ 1678     │ 12312     │
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
    │ Apply 4 filters and clear with 100        │  833                │  653                │ 149                 │  2093               │  833                │ 149                 │ 149      │  2741     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  833                │  653                │ 149                 │  2093               │  833                │ 149                 │ 149      │  2741     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  833                │  653                │ 149                 │  2093               │  833                │ 149                 │ 149      │  2741     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3873                │ 2893                │ 149                 │ 10733               │ 3873                │ 149                 │ 149      │ 14261     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3873                │ 2893                │ 149                 │ 10733               │ 3873                │ 149                 │ 149      │ 14261     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7673                │ 5693                │ 149                 │ 21533               │ 7673                │ 149                 │ 149      │ 28661     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   32                │   32                │  32                 │    32               │   32                │  32                 │  32      │    32     │
    │ Invert ordering of 100 element map        │   32                │   32                │  32                 │    32               │   32                │  32                 │  32      │    32     │
    │ Invert ordering of 101 element map        │   32                │   32                │  32                 │    32               │   32                │  32                 │  32      │    32     │
    │ Invert ordering of 1000 element map       │   32                │   32                │  32                 │    32               │   32                │  32                 │  32      │    32     │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │    0                │    0                │   0                 │     0               │    0                │   0                 │   0      │     0     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
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
