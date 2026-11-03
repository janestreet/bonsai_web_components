open! Core
open Bonsai_web_partial_render_table_configs_for_testing
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
    │ new (incr cells) (dynamic): 100    │  89        │  7372      │ 10112       │ 10112         │  7374            │ 0                 │
    │ new (incr rows) (dynamic): 100     │  85        │  2872      │  3612       │  3612         │  2874            │ 0                 │
    │ new (pure) (dynamic): 100          │  74        │   982      │  1422       │  1420         │   984            │ 0                 │
    │ new (incr cells) (static): 100     │  89        │  6873      │  7614       │  7613         │  6875            │ 0                 │
    │ new (incr rows) (static): 100      │  85        │  3373      │  4114       │  4113         │  3375            │ 0                 │
    │ new (pure) (static): 100           │  74        │   982      │  1423       │  1420         │   984            │ 0                 │
    │ dyn cols: 100                      │  75        │   978      │  1415       │  1416         │   980            │ 0                 │
    │ dyn cells: 100                     │ 104        │ 11232      │ 13493       │ 13491         │ 11234            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │  89        │  7443      │ 10210       │ 10210         │  7445            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │  85        │  2898      │  3645       │  3645         │  2900            │ 0                 │
    │ new (pure) (dynamic): 100000       │  74        │   989      │  1433       │  1431         │   991            │ 0                 │
    │ new (incr cells) (static): 100000  │  89        │  6939      │  7687       │  7686         │  6941            │ 0                 │
    │ new (incr rows) (static): 100000   │  85        │  3404      │  4152       │  4151         │  3406            │ 0                 │
    │ new (pure) (static): 100000        │  74        │   989      │  1434       │  1431         │   991            │ 0                 │
    │ dyn cols: 100000                   │  75        │   985      │  1426       │  1427         │   987            │ 0                 │
    │ dyn cells: 100000                  │ 104        │ 11341      │ 13624       │ 13622         │ 11343            │ 0                 │
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
    │ Focus by key (key not present) and        │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ Focus up and down in 100 element map      │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ Focus up and down in 101 element map      │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Focus up and down in 1000 element map     │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Focus up and down in 10000 element map    │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Focus left and right in a map with 10     │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ Page up and down in 100 element map       │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ Page up and down in 101 element map       │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Page up and down in 1000 element map      │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Page up and down in 10000 element map     │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Scroll 1-wide window from 0 to 9 and      │  341                │  296                │ 287                 │  337                │  302                │ 287                 │ 283      │   439     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  341                │  296                │ 287                 │  337                │  302                │ 287                 │ 283      │   439     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7370                │ 2870                │ 980                 │ 6871                │ 3371                │ 980                 │ 976      │ 11230     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3821                │ 1571                │ 631                 │ 3572                │ 1822                │ 631                 │ 627      │  5781     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3821                │ 1571                │ 631                 │ 3572                │ 1822                │ 631                 │ 627      │  5781     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  983                │  533                │ 353                 │  934                │  584                │ 353                 │ 349      │  1423     │
    │ Invert ordering of 100 element map        │ 7373                │ 2873                │ 983                 │ 6874                │ 3374                │ 983                 │ 979      │ 11233     │
    │ Invert ordering of 101 element map        │ 7444                │ 2899                │ 990                 │ 6940                │ 3405                │ 990                 │ 986      │ 11342     │
    │ Invert ordering of 1000 element map       │ 7444                │ 2899                │ 990                 │ 6940                │ 3405                │ 990                 │ 986      │ 11342     │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7370                │ 2870                │ 980                 │ 6871                │ 3371                │ 980                 │ 976      │ 11230     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  1287               │  247                │  23                 │   887               │  327                │  23                 │  23      │  1175     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1591               │  551                │ 199                 │  1191               │  631                │ 199                 │ 199      │  2119     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  1287               │  247                │  23                 │   887               │  327                │  23                 │  23      │  1175     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1591               │  551                │ 199                 │  1191               │  631                │ 199                 │ 199      │  2119     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  1591               │  551                │ 199                 │  1191               │  631                │ 199                 │ 199      │  2119     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3019               │  679                │ 175                 │  2119               │  859                │ 175                 │ 175      │  2767     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3019               │  679                │ 175                 │  2119               │  859                │ 175                 │ 175      │  2767     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3019               │  679                │ 175                 │  2119               │  859                │ 175                 │ 175      │  2767     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 15659               │ 2919                │ 175                 │ 10759               │ 3899                │ 175                 │ 175      │ 14287     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 15659               │ 2919                │ 175                 │ 10759               │ 3899                │ 175                 │ 175      │ 14287     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 31459               │ 5719                │ 175                 │ 21559               │ 7699                │ 175                 │ 175      │ 28687     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │    40               │   40                │  40                 │    40               │   40                │  40                 │  40      │    40     │
    │ Invert ordering of 100 element map        │    40               │   40                │  40                 │    40               │   40                │  40                 │  40      │    40     │
    │ Invert ordering of 101 element map        │    40               │   40                │  40                 │    40               │   40                │  40                 │  40      │    40     │
    │ Invert ordering of 1000 element map       │    40               │   40                │  40                 │    40               │   40                │  40                 │  40      │    40     │
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
    │ Focus by key (key not present) and        │   148               │   147               │  149                │   147               │   147               │  149                │  145     │   149     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   147               │   147               │  149                │   147               │   147               │  149                │  145     │   149     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   147               │   147               │  149                │   147               │   147               │  149                │  145     │   149     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   147               │   147               │  149                │   147               │   147               │  149                │  145     │   149     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   147               │   147               │  149                │   147               │   147               │  149                │  145     │   149     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   223               │   223               │  225                │   223               │   223               │  225                │  221     │   225     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   403               │   403               │  405                │   403               │   403               │  405                │  401     │   405     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   405               │   405               │  407                │   405               │   405               │  407                │  403     │   407     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   405               │   405               │  407                │   405               │   405               │  407                │  403     │   407     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   405               │   405               │  407                │   405               │   405               │  407                │  403     │   407     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │    70               │    70               │   72                │    70               │    70               │   72                │   70     │    70     │
    │ Focus up and down in 100 element map      │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Focus up and down in 101 element map      │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Focus up and down in 1000 element map     │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Focus up and down in 10000 element map    │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Focus left and right in a map with 10     │    70               │    70               │   72                │    70               │    70               │   72                │   70     │    70     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │    70               │    70               │   72                │    70               │    70               │   72                │   70     │    70     │
    │ Page up and down in 100 element map       │   160               │   160               │  162                │   160               │   160               │  162                │  160     │   160     │
    │ Page up and down in 101 element map       │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Page up and down in 1000 element map      │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Page up and down in 10000 element map     │   161               │   161               │  163                │   161               │   161               │  163                │  161     │   161     │
    │ Scroll 1-wide window from 0 to 9 and      │  2306               │  1586               │ 1331                │  2226               │  1666               │ 1331                │ 1331     │  3596     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2322               │  1602               │ 1347                │  2242               │  1682               │ 1347                │ 1347     │  3612     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  2306               │  1586               │ 1331                │  2226               │  1666               │ 1331                │ 1331     │  3596     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2322               │  1602               │ 1347                │  2242               │  1682               │ 1347                │ 1347     │  3612     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  2322               │  1602               │ 1347                │  2242               │  1682               │ 1347                │ 1347     │  3612     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3005               │  1385               │  715                │  2825               │  1565               │  715                │  715     │  4635     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3005               │  1385               │  715                │  2825               │  1565               │  715                │  715     │  4635     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3005               │  1385               │  715                │  2825               │  1565               │  715                │  715     │  4635     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14205               │  5385               │ 1675                │ 13225               │  6365               │ 1675                │ 1675     │ 21915     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14205               │  5385               │ 1675                │ 13225               │  6365               │ 1675                │ 1675     │ 21915     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28205               │ 10385               │ 2875                │ 26225               │ 12365               │ 2875                │ 2875     │ 43515     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   130               │   130               │  149                │   130               │   130               │  149                │  149     │   404     │
    │ Invert ordering of 100 element map        │   490               │   490               │  599                │   490               │   490               │  599                │  599     │  3014     │
    │ Invert ordering of 101 element map        │   494               │   494               │  604                │   494               │   494               │  604                │  604     │  3043     │
    │ Invert ordering of 1000 element map       │   494               │   494               │  604                │   494               │   494               │  604                │  604     │  3043     │
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
    │ Perform 10 sets of 1 items in a 10        │  1090               │   995               │  814                │  1185               │  1090               │  814                │  814     │  2214     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  1846               │  1571               │  994                │  2121               │  1846               │  994                │  994     │  4338     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  1090               │   995               │  814                │  1185               │  1090               │  814                │  814     │  2214     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  1846               │  1571               │  994                │  2121               │  1846               │  994                │  994     │  4338     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  1090               │   995               │  814                │  1185               │  1090               │  814                │  814     │  2214     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  1846               │  1571               │  994                │  2121               │  1846               │  994                │  994     │  4338     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  1090               │   995               │  814                │  1185               │  1090               │  814                │  814     │  2214     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  1846               │  1571               │  994                │  2121               │  1846               │  994                │  994     │  4338     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │  4681               │  3731               │ 1669                │  5631               │  4681               │ 1669                │ 1669     │ 12303     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  528                │  448                │ 125                 │  1088               │  528                │ 125                 │ 125      │  1871     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  238                │  238                │ 132                 │   238               │  238                │ 132                 │ 132      │   768     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  530                │  450                │ 126                 │  1090               │  530                │ 126                 │ 126      │  1878     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  240                │  240                │ 133                 │   240               │  240                │ 133                 │ 133      │   775     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │   60                │   60                │  43                 │    60               │   60                │  43                 │  43      │   145     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  857                │  677                │ 173                 │  2117               │  857                │ 173                 │ 173      │  2765     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  857                │  677                │ 173                 │  2117               │  857                │ 173                 │ 173      │  2765     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  857                │  677                │ 173                 │  2117               │  857                │ 173                 │ 173      │  2765     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3897                │ 2917                │ 173                 │ 10757               │ 3897                │ 173                 │ 173      │ 14285     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3897                │ 2917                │ 173                 │ 10757               │ 3897                │ 173                 │ 173      │ 14285     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7697                │ 5717                │ 173                 │ 21557               │ 7697                │ 173                 │ 173      │ 28685     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   38                │   38                │  38                 │    38               │   38                │  38                 │  38      │    38     │
    │ Invert ordering of 100 element map        │   38                │   38                │  38                 │    38               │   38                │  38                 │  38      │    38     │
    │ Invert ordering of 101 element map        │   38                │   38                │  38                 │    38               │   38                │  38                 │  38      │    38     │
    │ Invert ordering of 1000 element map       │   38                │   38                │  38                 │    38               │   38                │  38                 │  38      │    38     │
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
