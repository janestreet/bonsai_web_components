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
    │ new (incr cells) (dynamic): 100    │  89        │  7371      │ 10111       │ 10111         │  7373            │ 0                 │
    │ new (incr rows) (dynamic): 100     │  85        │  2871      │  3611       │  3611         │  2873            │ 0                 │
    │ new (pure) (dynamic): 100          │  74        │   981      │  1421       │  1419         │   983            │ 0                 │
    │ new (incr cells) (static): 100     │  89        │  6872      │  7613       │  7612         │  6874            │ 0                 │
    │ new (incr rows) (static): 100      │  85        │  3372      │  4113       │  4112         │  3374            │ 0                 │
    │ new (pure) (static): 100           │  74        │   981      │  1422       │  1419         │   983            │ 0                 │
    │ dyn cols: 100                      │  75        │   977      │  1414       │  1415         │   979            │ 0                 │
    │ dyn cells: 100                     │ 104        │ 11231      │ 13492       │ 13490         │ 11233            │ 0                 │
    │ new (incr cells) (dynamic): 100000 │  89        │  7442      │ 10209       │ 10209         │  7444            │ 0                 │
    │ new (incr rows) (dynamic): 100000  │  85        │  2897      │  3644       │  3644         │  2899            │ 0                 │
    │ new (pure) (dynamic): 100000       │  74        │   988      │  1432       │  1430         │   990            │ 0                 │
    │ new (incr cells) (static): 100000  │  89        │  6938      │  7686       │  7685         │  6940            │ 0                 │
    │ new (incr rows) (static): 100000   │  85        │  3403      │  4151       │  4150         │  3405            │ 0                 │
    │ new (pure) (static): 100000        │  74        │   988      │  1433       │  1430         │   990            │ 0                 │
    │ dyn cols: 100000                   │  75        │   984      │  1425       │  1426         │   986            │ 0                 │
    │ dyn cells: 100000                  │ 104        │ 11340      │ 13623       │ 13621         │ 11342            │ 0                 │
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
    │ Focus by key (key not present) and        │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ unfocus in 10 element map                 │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ unfocus in 100 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ unfocus in 101 element map                │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ unfocus in 1000 element map               │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key not present) and        │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ unfocus in 10000 element map              │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ in 10 element map                         │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ in 100 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ in 101 element map                        │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ in 1000 element map                       │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus by key (key present) and unfocus    │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ in 10000 element map                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus up and down in 10 element map       │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ Focus up and down in 100 element map      │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ Focus up and down in 101 element map      │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Focus up and down in 1000 element map     │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Focus up and down in 10000 element map    │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Focus left and right in a map with 10     │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 100    │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 101    │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 1000   │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Focus left and right in a map with 10000  │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ rows                                      │                     │                     │                     │                     │                     │                     │          │           │
    │ Page up and down in 10 element map        │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ Page up and down in 100 element map       │ 7371                │ 2871                │ 981                 │ 6872                │ 3372                │ 981                 │ 977      │ 11231     │
    │ Page up and down in 101 element map       │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Page up and down in 1000 element map      │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Page up and down in 10000 element map     │ 7442                │ 2897                │ 988                 │ 6938                │ 3403                │ 988                 │ 984      │ 11340     │
    │ Scroll 1-wide window from 0 to 9 and      │  340                │  295                │ 286                 │  336                │  301                │ 286                 │ 282      │   438     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  340                │  295                │ 286                 │  336                │  301                │ 286                 │ 282      │   438     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │ 7369                │ 2869                │ 979                 │ 6870                │ 3370                │ 979                 │ 975      │ 11229     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  980                │  530                │ 350                 │  931                │  581                │ 350                 │ 346      │  1420     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 3820                │ 1570                │ 630                 │ 3571                │ 1821                │ 630                 │ 626      │  5780     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 3820                │ 1570                │ 630                 │ 3571                │ 1821                │ 630                 │ 626      │  5780     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 7370                │ 2870                │ 980                 │ 6871                │ 3371                │ 980                 │ 976      │ 11230     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │  982                │  532                │ 352                 │  933                │  583                │ 352                 │ 348      │  1422     │
    │ Invert ordering of 100 element map        │ 7372                │ 2872                │ 982                 │ 6873                │ 3373                │ 982                 │ 978      │ 11232     │
    │ Invert ordering of 101 element map        │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Invert ordering of 1000 element map       │ 7443                │ 2898                │ 989                 │ 6939                │ 3404                │ 989                 │ 985      │ 11341     │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change one cell in it.                    │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 10 rows and a window of 10, then     │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 100 rows and a window of 10, then    │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Randomly select a row out of a table      │  981                │  531                │ 351                 │  932                │  582                │ 351                 │ 347      │  1421     │
    │ with 10000 rows and a window of 10, then  │                     │                     │                     │                     │                     │                     │          │           │
    │ change all cells in it.                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 10        │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  979                │  529                │ 349                 │  930                │  580                │ 349                 │ 345      │  1419     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │ 7369                │ 2869                │ 979                 │ 6870                │ 3370                │ 979                 │ 975      │ 11229     │
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
    │ Scroll 1-wide window from 0 to 9 and      │  2288               │  1568               │ 1313                │  2208               │  1648               │ 1313                │ 1313     │  3578     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2304               │  1584               │ 1329                │  2224               │  1664               │ 1329                │ 1329     │  3594     │
    │ back in 100 element map                   │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 1-wide window from 0 to 9 and      │  2288               │  1568               │ 1313                │  2208               │  1648               │ 1313                │ 1313     │  3578     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 10-wide window from 0 to 9 and     │  2304               │  1584               │ 1329                │  2224               │  1664               │ 1329                │ 1329     │  3594     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Scroll 100-wide window from 0 to 9 and    │  2304               │  1584               │ 1329                │  2224               │  1664               │ 1329                │ 1329     │  3594     │
    │ back in 1000 element map                  │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 100        │  3000               │  1380               │  710                │  2820               │  1560               │  710                │  710     │  4630     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 101        │  3000               │  1380               │  710                │  2820               │  1560               │  710                │  710     │  4630     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │  3000               │  1380               │  710                │  2820               │  1560               │  710                │  710     │  4630     │
    │ element map using 10 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 1000       │ 14200               │  5380               │ 1670                │ 13220               │  6360               │ 1670                │ 1670     │ 21910     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 14200               │  5380               │ 1670                │ 13220               │  6360               │ 1670                │ 1670     │ 21910     │
    │ element map using 50 window               │                     │                     │                     │                     │                     │                     │          │           │
    │ Apply 4 filters and clear with 10000      │ 28200               │ 10380               │ 2870                │ 26220               │ 12360               │ 2870                │ 2870     │ 43510     │
    │ element map using 100 window              │                     │                     │                     │                     │                     │                     │          │           │
    │ Invert ordering of 10 element map         │   129               │   129               │  148                │   129               │   129               │  148                │  148     │   403     │
    │ Invert ordering of 100 element map        │   489               │   489               │  598                │   489               │   489               │  598                │  598     │  3013     │
    │ Invert ordering of 101 element map        │   493               │   493               │  603                │   493               │   493               │  603                │  603     │  3042     │
    │ Invert ordering of 1000 element map       │   493               │   493               │  603                │   493               │   493               │  603                │  603     │  3042     │
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
    │ Perform 10 sets of 1 items in a 10        │  1079               │   984               │  803                │  1174               │  1079               │  803                │  803     │  2203     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 10        │  1835               │  1560               │  983                │  2110               │  1835               │  983                │  983     │  4327     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 11        │  1079               │   984               │  803                │  1174               │  1079               │  803                │  803     │  2203     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 11        │  1835               │  1560               │  983                │  2110               │  1835               │  983                │  983     │  4327     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 100       │  1079               │   984               │  803                │  1174               │  1079               │  803                │  803     │  2203     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 100       │  1835               │  1560               │  983                │  2110               │  1835               │  983                │  983     │  4327     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 1 items in a 1000      │  1079               │   984               │  803                │  1174               │  1079               │  803                │  803     │  2203     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 5 items in a 1000      │  1835               │  1560               │  983                │  2110               │  1835               │  983                │  983     │  4327     │
    │ element map with 10-wide window           │                     │                     │                     │                     │                     │                     │          │           │
    │ Perform 10 sets of 10 items in a 1000     │  4670               │  3720               │ 1658                │  5620               │  4670               │ 1658                │ 1658     │ 12292     │
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
