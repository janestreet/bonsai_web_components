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
    │ new (incr cells) (dynamic): 100    │ 78         │  7466      │ 10206       │ 10206         │  7468            │ 0                 │
    │ new (incr rows) (dynamic): 100     │ 74         │  2966      │  3706       │  3706         │  2968            │ 0                 │
    │ new (pure) (dynamic): 100          │ 63         │   975      │  1415       │  1413         │   977            │ 0                 │
    │ new (incr cells) (static): 100     │ 78         │  6967      │  7708       │  7707         │  6969            │ 0                 │
    │ new (incr rows) (static): 100      │ 74         │  3467      │  4208       │  4207         │  3469            │ 0                 │
    │ new (pure) (static): 100           │ 63         │   975      │  1416       │  1413         │   977            │ 0                 │
    │ dyn cols: 100                      │ 64         │   971      │  1408       │  1409         │   973            │ 0                 │
    │ dyn cells: 100                     │ 93         │ 11831      │ 14092       │ 14090         │ 11833            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │ 78         │  7538      │ 10305       │ 10305         │  7540            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │ 74         │  2993      │  3740       │  3740         │  2995            │ 0                 │
    │ new (pure) (dynamic): 100000       │ 63         │   982      │  1426       │  1424         │   984            │ 0                 │
    │ new (incr cells) (static): 100000  │ 78         │  7034      │  7782       │  7781         │  7036            │ 0                 │
    │ new (incr rows) (static): 100000   │ 74         │  3499      │  4247       │  4246         │  3501            │ 0                 │
    │ new (pure) (static): 100000        │ 63         │   982      │  1427       │  1424         │   984            │ 0                 │
    │ dyn cols: 100000                   │ 64         │   978      │  1419       │  1420         │   980            │ 0                 │
    │ dyn cells: 100000                  │ 93         │ 11946      │ 14229       │ 14227         │ 11948            │ 0                 │
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
    │ Focus by key (key not present) and        │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ Focus up and down in 100 element map      │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ Focus up and down in 101 element map      │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Focus up and down in 1000 element map     │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Focus up and down in 10000 element map    │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Focus left and right in a map with 10     │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ Page up and down in 100 element map       │ 7466                │ 2966                │ 975                 │ 6967                │ 3467                │ 975                 │ 971      │ 11831     │
    │ Page up and down in 101 element map       │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Page up and down in 1000 element map      │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Page up and down in 10000 element map     │ 7538                │ 2993                │ 982                 │ 7034                │ 3499                │ 982                 │ 978      │ 11946     │
    │ Scroll 1-wide window from 0 to 9 and      │  336                │  291                │ 280                 │  332                │  297                │ 280                 │ 276      │   444     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  336                │  291                │ 280                 │  332                │  297                │ 280                 │ 276      │   444     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7464                │ 2964                │ 973                 │ 6965                │ 3465                │ 973                 │ 969      │ 11829     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  985                │  535                │ 344                 │  936                │  586                │ 344                 │ 340      │  1480     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3865                │ 1615                │ 624                 │ 3616                │ 1866                │ 624                 │ 620      │  6080     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3865                │ 1615                │ 624                 │ 3616                │ 1866                │ 624                 │ 620      │  6080     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7465                │ 2965                │ 974                 │ 6966                │ 3466                │ 974                 │ 970      │ 11830     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  987                │  537                │ 346                 │  938                │  588                │ 346                 │ 342      │  1482     │
    │ Invert ordering of 100 element map        │ 7467                │ 2967                │ 976                 │ 6968                │ 3468                │ 976                 │ 972      │ 11832     │
    │ Invert ordering of 101 element map        │ 7539                │ 2994                │ 983                 │ 7035                │ 3500                │ 983                 │ 979      │ 11947     │
    │ Invert ordering of 1000 element map       │ 7539                │ 2994                │ 983                 │ 7035                │ 3500                │ 983                 │ 979      │ 11947     │
    │ Randomly select a row, then change one    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  986                │  536                │ 345                 │  937                │  587                │ 345                 │ 341      │  1481     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  984                │  534                │ 343                 │  935                │  585                │ 343                 │ 339      │  1479     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7464                │ 2964                │ 973                 │ 6965                │ 3465                │ 973                 │ 969      │ 11829     │
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
    │ Apply 4 filters and clear with 100        │  3012               │  1392               │  686                │  2832               │  1572               │  686                │  686     │  4822     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3012               │  1392               │  686                │  2832               │  1572               │  686                │  686     │  4822     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3012               │  1392               │  686                │  2832               │  1572               │  686                │  686     │  4822     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14372               │  5552               │ 1646                │ 13392               │  6532               │ 1646                │ 1646     │ 23062     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14372               │  5552               │ 1646                │ 13392               │  6532               │ 1646                │ 1646     │ 23062     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28572               │ 10752               │ 2846                │ 26592               │ 12732               │ 2846                │ 2846     │ 45862     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   123               │   123               │  142                │   123               │   123               │  142                │  142     │   397     │
    │ Invert ordering of 100 element map        │   483               │   483               │  592                │   483               │   483               │  592                │  592     │  3007     │
    │ Invert ordering of 101 element map        │   487               │   487               │  597                │   487               │   487               │  597                │  597     │  3036     │
    │ Invert ordering of 1000 element map       │   487               │   487               │  597                │   487               │   487               │  597                │  597     │  3036     │
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
