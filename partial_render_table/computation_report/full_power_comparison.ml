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
    │ new (incr cells) (dynamic): 100    │  86        │  7477      │ 10217       │ 10217         │  7479            │ 0                 │
    │ new (incr rows) (dynamic): 100     │  82        │  2977      │  3717       │  3717         │  2979            │ 0                 │
    │ new (pure) (dynamic): 100          │  71        │   986      │  1426       │  1424         │   988            │ 0                 │
    │ new (incr cells) (static): 100     │  86        │  6978      │  7719       │  7718         │  6980            │ 0                 │
    │ new (incr rows) (static): 100      │  82        │  3478      │  4219       │  4218         │  3480            │ 0                 │
    │ new (pure) (static): 100           │  71        │   986      │  1427       │  1424         │   988            │ 0                 │
    │ dyn cols: 100                      │  72        │   982      │  1419       │  1420         │   984            │ 0                 │
    │ dyn cells: 100                     │ 101        │ 11842      │ 14103       │ 14101         │ 11844            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │  86        │  7549      │ 10316       │ 10316         │  7551            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │  82        │  3004      │  3751       │  3751         │  3006            │ 0                 │
    │ new (pure) (dynamic): 100000       │  71        │   993      │  1437       │  1435         │   995            │ 0                 │
    │ new (incr cells) (static): 100000  │  86        │  7045      │  7793       │  7792         │  7047            │ 0                 │
    │ new (incr rows) (static): 100000   │  82        │  3510      │  4258       │  4257         │  3512            │ 0                 │
    │ new (pure) (static): 100000        │  71        │   993      │  1438       │  1435         │   995            │ 0                 │
    │ dyn cols: 100000                   │  72        │   989      │  1430       │  1431         │   991            │ 0                 │
    │ dyn cells: 100000                  │ 101        │ 11957      │ 14240       │ 14238         │ 11959            │ 0                 │
    └────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┴──────────────────┴───────────────────┘

    ======= Startup Incr Annotated Node Counts =======
    ┌────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬─────────────────────┐
    │                            │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle_apply_act │
    │                            │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ ion_pair            │
    ├────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼─────────────────────┤
    │ new (incr cells)           │ 9     │ 3314  │ 7046   │ 20        │ 803       │  724  │ 700       │ 3         │ 800       │ 800       │ 102       │ 102       │ 101       │ 1    │ 0                   │
    │ (dynamic): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows)            │ 9     │ 1314  │ 2646   │ 20        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (dynamic): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (dynamic): 100  │ 9     │  420  │  760   │ 24        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ new (incr cells)           │ 9     │ 3814  │ 8646   │ 20        │ 203       │ 1224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (static): 100              │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows) (static):  │ 9     │ 1814  │ 3646   │ 20        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0                   │
    │ 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (static): 100   │ 9     │  419  │  758   │ 24        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cols: 100              │ 9     │  416  │  752   │ 20        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cells: 100             │ 9     │ 4638  │ 9793   │ 20        │ 704       │ 1230  │ 100       │ 4         │ 700       │ 700       │   7       │   7       │   1       │ 1    │ 0                   │
    │ new (incr cells)           │ 9     │ 3346  │ 7114   │ 20        │ 811       │  731  │ 707       │ 3         │ 808       │ 808       │ 103       │ 103       │ 102       │ 1    │ 0                   │
    │ (dynamic): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows)            │ 9     │ 1326  │ 2670   │ 20        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (dynamic): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (dynamic):      │ 9     │  423  │  765   │ 24        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr cells)           │ 9     │ 3851  │ 8730   │ 20        │ 205       │ 1236  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ (static): 100000           │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (incr rows) (static):  │ 9     │ 1831  │ 3680   │ 20        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ new (pure) (static):       │ 9     │  422  │  763   │ 24        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │                     │
    │ dyn cols: 100000           │ 9     │  419  │  757   │ 20        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0                   │
    │ dyn cells: 100000          │ 9     │ 4683  │ 9888   │ 20        │ 711       │ 1242  │ 101       │ 4         │ 707       │ 707       │   7       │   7       │   1       │ 1    │ 0                   │
    └────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴─────────────────────┘

    ======= Bonsai Computation Nodes =======
    ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
    │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ comput │
    │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ ation_ │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ watche │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ r      │
    ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
    │ new (incr cells)    │ 119    │ 0      │ 1     │ 10    │  2     │ 0      │ 134 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 120    │ 0      │ 1     │ 10    │  2     │ 0      │ 135 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 133 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr cells)    │ 146    │ 0      │ 1     │ 15    │  2     │ 0      │ 165 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 126    │ 0      │ 1     │ 10    │  2     │ 0      │ 140 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 132 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols: 100       │ 114    │ 0      │ 1     │ 10    │  3     │ 0      │ 129 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ dyn cells: 100      │ 171    │ 0      │ 1     │ 16    │ 10     │ 0      │ 198 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ new (incr cells)    │ 119    │ 0      │ 1     │ 10    │  2     │ 0      │ 134 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 120    │ 0      │ 1     │ 10    │  2     │ 0      │ 135 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 133 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic): 100000   │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr cells)    │ 146    │ 0      │ 1     │ 15    │  2     │ 0      │ 165 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (incr rows)     │ 126    │ 0      │ 1     │ 10    │  2     │ 0      │ 140 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new (pure)          │ 117    │ 0      │ 1     │ 10    │  3     │ 0      │ 132 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols: 100000    │ 114    │ 0      │ 1     │ 10    │  3     │ 0      │ 129 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ dyn cells: 100000   │ 171    │ 0      │ 1     │ 16    │ 10     │ 0      │ 198 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

    ======= Bonsai Value Nodes =======
    ┌────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
    │                                    │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
    ├────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
    │ new (incr cells) (dynamic): 100    │ 3        │ 0          │ 6    │ 230   │ 20     │ 125  │
    │ new (incr rows) (dynamic): 100     │ 3        │ 0          │ 6    │ 230   │ 20     │ 126  │
    │ new (pure) (dynamic): 100          │ 3        │ 0          │ 6    │ 225   │ 20     │ 123  │
    │ new (incr cells) (static): 100     │ 3        │ 0          │ 6    │ 268   │ 20     │ 152  │
    │ new (incr rows) (static): 100      │ 3        │ 0          │ 6    │ 238   │ 20     │ 132  │
    │ new (pure) (static): 100           │ 3        │ 0          │ 6    │ 223   │ 20     │ 123  │
    │ dyn cols: 100                      │ 3        │ 0          │ 4    │ 221   │ 20     │ 120  │
    │ dyn cells: 100                     │ 3        │ 0          │ 8    │ 316   │ 20     │ 177  │
    │ new (incr cells) (dynamic): 100000 │ 3        │ 0          │ 6    │ 230   │ 20     │ 125  │
    │ new (incr rows) (dynamic): 100000  │ 3        │ 0          │ 6    │ 230   │ 20     │ 126  │
    │ new (pure) (dynamic): 100000       │ 3        │ 0          │ 6    │ 225   │ 20     │ 123  │
    │ new (incr cells) (static): 100000  │ 3        │ 0          │ 6    │ 268   │ 20     │ 152  │
    │ new (incr rows) (static): 100000   │ 3        │ 0          │ 6    │ 238   │ 20     │ 132  │
    │ new (pure) (static): 100000        │ 3        │ 0          │ 6    │ 223   │ 20     │ 123  │
    │ dyn cols: 100000                   │ 3        │ 0          │ 4    │ 221   │ 20     │ 120  │
    │ dyn cells: 100000                  │ 3        │ 0          │ 8    │ 316   │ 20     │ 177  │
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
    │ Focus by key (key not present) and        │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7477                │ 2977                │ 986                 │ 6978                │ 3478                │ 986                 │ 982      │ 11842     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7477                │ 2977                │ 986                 │ 6978                │ 3478                │ 986                 │ 982      │ 11842     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ Focus up and down in 100 element map      │ 7477                │ 2977                │ 986                 │ 6978                │ 3478                │ 986                 │ 982      │ 11842     │
    │ Focus up and down in 101 element map      │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Focus up and down in 1000 element map     │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Focus up and down in 10000 element map    │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Focus left and right in a map with 10     │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7477                │ 2977                │ 986                 │ 6978                │ 3478                │ 986                 │ 982      │ 11842     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ Page up and down in 100 element map       │ 7477                │ 2977                │ 986                 │ 6978                │ 3478                │ 986                 │ 982      │ 11842     │
    │ Page up and down in 101 element map       │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Page up and down in 1000 element map      │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Page up and down in 10000 element map     │ 7549                │ 3004                │ 993                 │ 7045                │ 3510                │ 993                 │ 989      │ 11957     │
    │ Scroll 1-wide window from 0 to 9 and      │  351                │  306                │ 295                 │  347                │  312                │ 295                 │ 291      │   459     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  999                │  549                │ 358                 │  950                │  600                │ 358                 │ 354      │  1494     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  351                │  306                │ 295                 │  347                │  312                │ 295                 │ 291      │   459     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  999                │  549                │ 358                 │  950                │  600                │ 358                 │ 354      │  1494     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7479                │ 2979                │ 988                 │ 6980                │ 3480                │ 988                 │ 984      │ 11844     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  996                │  546                │ 355                 │  947                │  597                │ 355                 │ 351      │  1491     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  996                │  546                │ 355                 │  947                │  597                │ 355                 │ 351      │  1491     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  996                │  546                │ 355                 │  947                │  597                │ 355                 │ 351      │  1491     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3876                │ 1626                │ 635                 │ 3627                │ 1877                │ 635                 │ 631      │  6091     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3876                │ 1626                │ 635                 │ 3627                │ 1877                │ 635                 │ 631      │  6091     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7476                │ 2976                │ 985                 │ 6977                │ 3477                │ 985                 │ 981      │ 11841     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  998                │  548                │ 357                 │  949                │  599                │ 357                 │ 353      │  1493     │
    │ Invert ordering of 100 element map        │ 7478                │ 2978                │ 987                 │ 6979                │ 3479                │ 987                 │ 983      │ 11843     │
    │ Invert ordering of 101 element map        │ 7550                │ 3005                │ 994                 │ 7046                │ 3511                │ 994                 │ 990      │ 11958     │
    │ Invert ordering of 1000 element map       │ 7550                │ 3005                │ 994                 │ 7046                │ 3511                │ 994                 │ 990      │ 11958     │
    │ Randomly select a row, then change one    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change one    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cell in it.                               │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row, then change all    │  997                │  547                │ 356                 │  948                │  598                │ 356                 │ 352      │  1492     │
    │ cells in it.                              │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  995                │  545                │ 354                 │  946                │  596                │ 354                 │ 350      │  1490     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7475                │ 2975                │ 984                 │ 6976                │ 3476                │ 984                 │ 980      │ 11840     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  1320               │  280                │  40                 │   920               │  360                │  40                 │  40      │  1288     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1624               │  584                │ 216                 │  1224               │  664                │ 216                 │ 216      │  2232     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  1320               │  280                │  40                 │   920               │  360                │  40                 │  40      │  1288     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  1624               │  584                │ 216                 │  1224               │  664                │ 216                 │ 216      │  2232     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  1624               │  584                │ 216                 │  1224               │  664                │ 216                 │ 216      │  2232     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3091               │  751                │ 211                 │  2191               │  931                │ 211                 │ 211      │  3019     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3091               │  751                │ 211                 │  2191               │  931                │ 211                 │ 211      │  3019     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3091               │  751                │ 211                 │  2191               │  931                │ 211                 │ 211      │  3019     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 15891               │ 3151                │ 211                 │ 10991               │ 4131                │ 211                 │ 211      │ 15499     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 15891               │ 3151                │ 211                 │ 10991               │ 4131                │ 211                 │ 211      │ 15499     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 31891               │ 6151                │ 211                 │ 21991               │ 8131                │ 211                 │ 211      │ 31099     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │    49               │   49                │  49                 │    49               │   49                │  49                 │  49      │    49     │
    │ Invert ordering of 100 element map        │    49               │   49                │  49                 │    49               │   49                │  49                 │  49      │    49     │
    │ Invert ordering of 101 element map        │    49               │   49                │  49                 │    49               │   49                │  49                 │  49      │    49     │
    │ Invert ordering of 1000 element map       │    49               │   49                │  49                 │    49               │   49                │  49                 │  49      │    49     │
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
    │ Focus by key (key not present) and        │   147               │   146               │  142                │   146               │   146               │  142                │  146     │   146     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   146               │   146               │  142                │   146               │   146               │  142                │  146     │   146     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   146               │   146               │  142                │   146               │   146               │  142                │  146     │   146     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   146               │   146               │  142                │   146               │   146               │  142                │  146     │   146     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │   146               │   146               │  142                │   146               │   146               │  142                │  146     │   146     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   222               │   222               │  218                │   222               │   222               │  218                │  222     │   222     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   402               │   402               │  398                │   402               │   402               │  398                │  402     │   402     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   404               │   404               │  400                │   404               │   404               │  400                │  404     │   404     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   404               │   404               │  400                │   404               │   404               │  400                │  404     │   404     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │   404               │   404               │  400                │   404               │   404               │  400                │  404     │   404     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │    71               │    71               │   69                │    71               │    71               │   69                │   71     │    71     │
    │ Focus up and down in 100 element map      │   161               │   161               │  159                │   161               │   161               │  159                │  161     │   161     │
    │ Focus up and down in 101 element map      │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Focus up and down in 1000 element map     │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Focus up and down in 10000 element map    │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Focus left and right in a map with 10     │    71               │    71               │   69                │    71               │    71               │   69                │   71     │    71     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │   161               │   161               │  159                │   161               │   161               │  159                │  161     │   161     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │    71               │    71               │   69                │    71               │    71               │   69                │   71     │    71     │
    │ Page up and down in 100 element map       │   161               │   161               │  159                │   161               │   161               │  159                │  161     │   161     │
    │ Page up and down in 101 element map       │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Page up and down in 1000 element map      │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Page up and down in 10000 element map     │   162               │   162               │  160                │   162               │   162               │  160                │  162     │   162     │
    │ Scroll 1-wide window from 0 to 9 and      │  2644               │  1924               │ 1655                │  2564               │  2004               │ 1655                │ 1655     │  4012     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2660               │  1940               │ 1671                │  2580               │  2020               │ 1671                │ 1671     │  4028     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  2644               │  1924               │ 1655                │  2564               │  2004               │ 1655                │ 1655     │  4012     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2660               │  1940               │ 1671                │  2580               │  2020               │ 1671                │ 1671     │  4028     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  2660               │  1940               │ 1671                │  2580               │  2020               │ 1671                │ 1671     │  4028     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3062               │  1442               │  738                │  2882               │  1622               │  738                │  738     │  4870     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3062               │  1442               │  738                │  2882               │  1622               │  738                │  738     │  4870     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3062               │  1442               │  738                │  2882               │  1622               │  738                │  738     │  4870     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14422               │  5602               │ 1698                │ 13442               │  6582               │ 1698                │ 1698     │ 23110     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14422               │  5602               │ 1698                │ 13442               │  6582               │ 1698                │ 1698     │ 23110     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28622               │ 10802               │ 2898                │ 26642               │ 12782               │ 2898                │ 2898     │ 45910     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   136               │   136               │  155                │   136               │   136               │  155                │  155     │   410     │
    │ Invert ordering of 100 element map        │   496               │   496               │  605                │   496               │   496               │  605                │  605     │  3020     │
    │ Invert ordering of 101 element map        │   500               │   500               │  610                │   500               │   500               │  610                │  610     │  3049     │
    │ Invert ordering of 1000 element map       │   500               │   500               │  610                │   500               │   500               │  610                │  610     │  3049     │
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
    │ Perform 10 sets of 1 items in a 10        │  1127               │  1032               │  853                │  1222               │  1127               │  853                │  853     │  2249     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  1883               │  1608               │ 1033                │  2158               │  1883               │ 1033                │ 1033     │  4373     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  1127               │  1032               │  853                │  1222               │  1127               │  853                │  853     │  2249     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  1883               │  1608               │ 1033                │  2158               │  1883               │ 1033                │ 1033     │  4373     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  1127               │  1032               │  853                │  1222               │  1127               │  853                │  853     │  2249     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  1883               │  1608               │ 1033                │  2158               │  1883               │ 1033                │ 1033     │  4373     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  1127               │  1032               │  853                │  1222               │  1127               │  853                │  853     │  2249     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  1883               │  1608               │ 1033                │  2158               │  1883               │ 1033                │ 1033     │  4373     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │  4718               │  3768               │ 1708                │  5668               │  4718               │ 1708                │ 1708     │ 12338     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  541                │  461                │ 138                 │  1101               │  541                │ 138                 │ 138      │  1884     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  251                │  251                │ 145                 │   251               │  251                │ 145                 │ 145      │   781     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  543                │  463                │ 139                 │  1103               │  543                │ 139                 │ 139      │  1891     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  253                │  253                │ 146                 │   253               │  253                │ 146                 │ 146      │   788     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │   73                │   73                │  56                 │    73               │   73                │  56                 │  56      │   158     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  893                │  713                │ 209                 │  2153               │  893                │ 209                 │ 209      │  2801     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  893                │  713                │ 209                 │  2153               │  893                │ 209                 │ 209      │  2801     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  893                │  713                │ 209                 │  2153               │  893                │ 209                 │ 209      │  2801     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3933                │ 2953                │ 209                 │ 10793               │ 3933                │ 209                 │ 209      │ 14321     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3933                │ 2953                │ 209                 │ 10793               │ 3933                │ 209                 │ 209      │ 14321     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7733                │ 5753                │ 209                 │ 21593               │ 7733                │ 209                 │ 209      │ 28721     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   47                │   47                │  47                 │    47               │   47                │  47                 │  47      │    47     │
    │ Invert ordering of 100 element map        │   47                │   47                │  47                 │    47               │   47                │  47                 │  47      │    47     │
    │ Invert ordering of 101 element map        │   47                │   47                │  47                 │    47               │   47                │  47                 │  47      │    47     │
    │ Invert ordering of 1000 element map       │   47                │   47                │  47                 │    47               │   47                │  47                 │  47      │    47     │
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
