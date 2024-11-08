open! Core
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Report = Bonsai_web_test.Computation_report

(* This test compares the "most powerful" version of each API.

   These are the tests where we want to focus most on improving our numbers. *)

let test_startup configs =
  let startup_inputs =
    List.map [ 100; 100_000 ] ~f:(fun size ->
      Int.to_string size, Prt_input.create (Row.init_rows size))
  in
  Report.Startup.run_and_print_compare (module Config) startup_inputs configs
;;

let%expect_test "" =
  let configs = Config.full_power_comparison in
  test_startup configs;
  [%expect
    {|
    ======= Startup Incr Node Stats =======
    ┌───────────────────────────────────────────────────────────┬────────────┬────────────┬─────────────┬───────────────┬──────────────────┬───────────────────┐
    │                                                           │ max_height │ node_count │ max_node_id │ nodes_created │ nodes_recomputed │ nodes_invalidated │
    ├───────────────────────────────────────────────────────────┼────────────┼────────────┼─────────────┼───────────────┼──────────────────┼───────────────────┤
    │ new incr_cells (dynamic cols) (counters) (groups): 100    │ 82         │  7567      │ 10305       │ 10305         │  7569            │ 0                 │
    │ new incr_rows (dynamic cols) (counters) (groups): 100     │ 78         │  3067      │  3805       │  3805         │  3069            │ 0                 │
    │ new pure (dynamic cols) (counters) (groups): 100          │ 67         │  1076      │  1514       │  1512         │  1078            │ 0                 │
    │ new incr_cells (static cols) (counters) (groups): 100     │ 82         │  7068      │  7807       │  7806         │  7070            │ 0                 │
    │ new incr_rows (static cols) (counters) (groups): 100      │ 78         │  3568      │  4307       │  4306         │  3570            │ 0                 │
    │ new pure (static cols) (counters) (groups): 100           │ 67         │  1076      │  1515       │  1512         │  1078            │ 0                 │
    │ dyn cols (counters) (groups): 100                         │ 68         │  1072      │  1507       │  1508         │  1074            │ 0                 │
    │ dyn cells (counters) (groups): 100                        │ 97         │ 11936      │ 14195       │ 14193         │ 11938            │ 0                 │
    │ new incr_cells (dynamic cols) (counters) (groups): 100000 │ 82         │  7640      │ 10405       │ 10405         │  7642            │ 0                 │
    │ new incr_rows (dynamic cols) (counters) (groups): 100000  │ 78         │  3095      │  3840       │  3840         │  3097            │ 0                 │
    │ new pure (dynamic cols) (counters) (groups): 100000       │ 67         │  1084      │  1526       │  1524         │  1086            │ 0                 │
    │ new incr_cells (static cols) (counters) (groups): 100000  │ 82         │  7136      │  7882       │  7881         │  7138            │ 0                 │
    │ new incr_rows (static cols) (counters) (groups): 100000   │ 78         │  3601      │  4347       │  4346         │  3603            │ 0                 │
    │ new pure (static cols) (counters) (groups): 100000        │ 67         │  1084      │  1527       │  1524         │  1086            │ 0                 │
    │ dyn cols (counters) (groups): 100000                      │ 68         │  1080      │  1519       │  1520         │  1082            │ 0                 │
    │ dyn cells (counters) (groups): 100000                     │ 97         │ 12052      │ 14333       │ 14331         │ 12054            │ 0                 │
    └───────────────────────────────────────────────────────────┴────────────┴────────────┴─────────────┴───────────────┴──────────────────┴───────────────────┘

    ======= Startup Incr Annotated Node Counts =======
    ┌──────────────────────────────────────┬───────┬───────┬────────┬───────────┬───────────┬───────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬───────────┬──────┬───────────┐
    │                                      │ input │ value │ result │ lifecycle │ empty_lif │ model │ model_and │ switch_mo │ assoc_key │ assoc_inp │ assoc_res │ assoc_lif │ assoc_inp │ path │ lifecycle │
    │                                      │       │       │        │           │ ecycle    │       │ _input    │ del       │           │ ut        │ ults      │ ecycles   │ uts       │      │ _apply_ac │
    │                                      │       │       │        │           │           │       │           │           │           │           │           │           │           │      │ tion_pair │
    ├──────────────────────────────────────┼───────┼───────┼────────┼───────────┼───────────┼───────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼───────────┼──────┼───────────┤
    │ new incr_cells (dynamic cols)        │ 9     │ 3312  │ 7042   │ 19        │ 803       │  724  │ 700       │ 3         │ 800       │ 800       │ 102       │ 102       │ 101       │ 1    │ 0         │
    │ (counters) (groups): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_rows (dynamic cols)         │ 9     │ 1312  │ 2642   │ 19        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new pure (dynamic cols) (counters)   │ 9     │  418  │  756   │ 24        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0         │
    │ (groups): 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_cells (static cols)         │ 9     │ 3812  │ 8642   │ 19        │ 203       │ 1224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_rows (static cols)          │ 9     │ 1812  │ 3642   │ 19        │ 203       │  224  │ 100       │ 3         │ 200       │ 200       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100             │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new pure (static cols) (counters)    │ 9     │  417  │  754   │ 24        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0         │
    │ (groups): 100                        │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ dyn cols (counters) (groups): 100    │ 9     │  414  │  748   │ 20        │ 103       │   25  │ 100       │ 3         │ 100       │ 100       │   1       │   1       │   1       │ 1    │ 0         │
    │ dyn cells (counters) (groups): 100   │ 9     │ 4640  │ 9797   │ 23        │ 704       │ 1230  │ 100       │ 4         │ 700       │ 700       │   7       │   7       │   1       │ 1    │ 0         │
    │ new incr_cells (dynamic cols)        │ 9     │ 3344  │ 7110   │ 19        │ 811       │  731  │ 707       │ 3         │ 808       │ 808       │ 103       │ 103       │ 102       │ 1    │ 0         │
    │ (counters) (groups): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_rows (dynamic cols)         │ 9     │ 1324  │ 2666   │ 19        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new pure (dynamic cols) (counters)   │ 9     │  421  │  761   │ 24        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0         │
    │ (groups): 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_cells (static cols)         │ 9     │ 3849  │ 8726   │ 19        │ 205       │ 1236  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new incr_rows (static cols)          │ 9     │ 1829  │ 3676   │ 19        │ 205       │  226  │ 101       │ 3         │ 202       │ 202       │   2       │   2       │   1       │ 1    │ 0         │
    │ (counters) (groups): 100000          │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ new pure (static cols) (counters)    │ 9     │  420  │  759   │ 24        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0         │
    │ (groups): 100000                     │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    │ dyn cols (counters) (groups): 100000 │ 9     │  417  │  753   │ 20        │ 104       │   25  │ 101       │ 3         │ 101       │ 101       │   1       │   1       │   1       │ 1    │ 0         │
    │ dyn cells (counters) (groups):       │ 9     │ 4685  │ 9892   │ 23        │ 711       │ 1242  │ 101       │ 4         │ 707       │ 707       │   7       │   7       │   1       │ 1    │ 0         │
    │ 100000                               │       │       │        │           │           │       │           │           │           │           │           │           │           │      │           │
    └──────────────────────────────────────┴───────┴───────┴────────┴───────────┴───────────┴───────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴───────────┴──────┴───────────┘

    ======= Bonsai Computation Nodes =======
    ┌─────────────────────┬────────┬────────┬───────┬───────┬────────┬────────┬─────┬───────┬───────┬───────┬────────┬────────┬────────┬────────┬────────┬──────┬────────┬──────┬────────┬────────┬────────┐
    │                     │ return │ leaf01 │ leaf1 │ leaf0 │ leaf_i │ model_ │ sub │ store │ fetch │ assoc │ assoc_ │ assoc_ │ switch │ fix_de │ fix_re │ wrap │ with_m │ path │ lifecy │ identi │ comput │
    │                     │        │        │       │       │ ncr    │ cutoff │     │       │       │       │ on     │ simpl  │        │ fine   │ curse  │      │ odel_r │      │ cle    │ ty     │ ation_ │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ esette │      │        │        │ watche │
    │                     │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │ r      │      │        │        │ r      │
    ├─────────────────────┼────────┼────────┼───────┼───────┼────────┼────────┼─────┼───────┼───────┼───────┼────────┼────────┼────────┼────────┼────────┼──────┼────────┼──────┼────────┼────────┼────────┤
    │ new incr_cells      │ 117    │ 0      │ 1     │ 10    │  2     │ 0      │ 132 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic cols)      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_rows       │ 118    │ 0      │ 1     │ 10    │  2     │ 0      │ 133 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic cols)      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new pure (dynamic   │ 115    │ 0      │ 1     │ 10    │  3     │ 0      │ 131 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ cols) (counters)    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_cells      │ 144    │ 0      │ 1     │ 15    │  2     │ 0      │ 163 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static cols)       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_rows       │ 124    │ 0      │ 1     │ 10    │  2     │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static cols)       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new pure (static    │ 115    │ 0      │ 1     │ 10    │  3     │ 0      │ 130 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ cols) (counters)    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols            │ 112    │ 0      │ 1     │ 10    │  3     │ 0      │ 127 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cells           │ 173    │ 0      │ 1     │ 16    │ 10     │ 0      │ 200 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_cells      │ 117    │ 0      │ 1     │ 10    │  2     │ 0      │ 132 │ 0     │ 1     │ 2     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic cols)      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_rows       │ 118    │ 0      │ 1     │ 10    │  2     │ 0      │ 133 │ 0     │ 1     │ 1     │ 1      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (dynamic cols)      │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new pure (dynamic   │ 115    │ 0      │ 1     │ 10    │  3     │ 0      │ 131 │ 0     │ 1     │ 1     │ 0      │ 1      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ cols) (counters)    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_cells      │ 144    │ 0      │ 1     │ 15    │  2     │ 0      │ 163 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static cols)       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new incr_rows       │ 124    │ 0      │ 1     │ 10    │  2     │ 0      │ 138 │ 0     │ 1     │ 1     │ 1      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (static cols)       │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ new pure (static    │ 115    │ 0      │ 1     │ 10    │  3     │ 0      │ 130 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ cols) (counters)    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cols            │ 112    │ 0      │ 1     │ 10    │  3     │ 0      │ 127 │ 0     │ 1     │ 1     │ 0      │ 0      │ 3      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ dyn cells           │ 173    │ 0      │ 1     │ 16    │ 10     │ 0      │ 200 │ 0     │ 1     │ 1     │ 7      │ 0      │ 4      │ 0      │ 0      │ 0    │ 0      │ 1    │ 3      │ 0      │ 0      │
    │ (counters)          │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    │ (groups): 100000    │        │        │       │       │        │        │     │       │       │       │        │        │        │        │        │      │        │      │        │        │        │
    └─────────────────────┴────────┴────────┴───────┴───────┴────────┴────────┴─────┴───────┴───────┴───────┴────────┴────────┴────────┴────────┴────────┴──────┴────────┴──────┴────────┴────────┴────────┘

    ======= Bonsai Value Nodes =======
    ┌───────────────────────────────────────────────────────────┬──────────┬────────────┬──────┬───────┬────────┬──────┐
    │                                                           │ constant │ exception_ │ incr │ named │ cutoff │ mapn │
    ├───────────────────────────────────────────────────────────┼──────────┼────────────┼──────┼───────┼────────┼──────┤
    │ new incr_cells (dynamic cols) (counters) (groups): 100    │ 2        │ 0          │ 6    │ 225   │ 21     │ 123  │
    │ new incr_rows (dynamic cols) (counters) (groups): 100     │ 2        │ 0          │ 6    │ 225   │ 21     │ 124  │
    │ new pure (dynamic cols) (counters) (groups): 100          │ 2        │ 0          │ 6    │ 220   │ 21     │ 121  │
    │ new incr_cells (static cols) (counters) (groups): 100     │ 2        │ 0          │ 6    │ 263   │ 21     │ 150  │
    │ new incr_rows (static cols) (counters) (groups): 100      │ 2        │ 0          │ 6    │ 233   │ 21     │ 130  │
    │ new pure (static cols) (counters) (groups): 100           │ 2        │ 0          │ 6    │ 218   │ 21     │ 121  │
    │ dyn cols (counters) (groups): 100                         │ 2        │ 0          │ 4    │ 216   │ 21     │ 118  │
    │ dyn cells (counters) (groups): 100                        │ 2        │ 0          │ 8    │ 315   │ 21     │ 179  │
    │ new incr_cells (dynamic cols) (counters) (groups): 100000 │ 2        │ 0          │ 6    │ 225   │ 21     │ 123  │
    │ new incr_rows (dynamic cols) (counters) (groups): 100000  │ 2        │ 0          │ 6    │ 225   │ 21     │ 124  │
    │ new pure (dynamic cols) (counters) (groups): 100000       │ 2        │ 0          │ 6    │ 220   │ 21     │ 121  │
    │ new incr_cells (static cols) (counters) (groups): 100000  │ 2        │ 0          │ 6    │ 263   │ 21     │ 150  │
    │ new incr_rows (static cols) (counters) (groups): 100000   │ 2        │ 0          │ 6    │ 233   │ 21     │ 130  │
    │ new pure (static cols) (counters) (groups): 100000        │ 2        │ 0          │ 6    │ 218   │ 21     │ 121  │
    │ dyn cols (counters) (groups): 100000                      │ 2        │ 0          │ 4    │ 216   │ 21     │ 118  │
    │ dyn cells (counters) (groups): 100000                     │ 2        │ 0          │ 8    │ 315   │ 21     │ 179  │
    └───────────────────────────────────────────────────────────┴──────────┴────────────┴──────┴───────┴────────┴──────┘
    |}];
  Report.Interaction.run_and_print_compare (module Config) scenarios configs;
  [%expect
    {|
    ====== Node Count ======
    ┌──────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┐
    │                      │ new incr_cells      │ new incr_rows       │ new pure (dynamic   │ new incr_cells      │ new incr_rows       │ new pure (static    │ dyn cols            │ dyn cells           │
    │                      │ (dynamic cols)      │ (dynamic cols)      │ cols) (counters)    │ (static cols)       │ (static cols)       │ cols) (counters)    │ (counters) (groups) │ (counters) (groups) │
    │                      │ (counters) (groups) │ (counters) (groups) │ (groups)            │ (counters) (groups) │ (counters) (groups) │ (groups)            │                     │                     │
    ├──────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┤
    │ Focus by key (key    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ in 10 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ in 100 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ in 101 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ in 1000 element map  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ in 10000 element map │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 rows              │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 101 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 1000 rows            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10000 rows           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │ 7640                │ 3095                │ 1084                │ 7136                │ 3601                │ 1084                │ 1080                │ 12052               │
    │ 10000 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  344                │  299                │  288                │  340                │  305                │  288                │  284                │   456               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │ 1001                │  551                │  360                │  952                │  602                │  360                │  356                │  1500               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  344                │  299                │  288                │  340                │  305                │  288                │  284                │   456               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │ 1001                │  551                │  360                │  952                │  602                │  360                │  356                │  1500               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 100-wide      │ 7571                │ 3071                │ 1080                │ 7072                │ 3572                │ 1080                │ 1076                │ 11940               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  998                │  548                │  357                │  949                │  599                │  357                │  353                │  1497               │
    │ clear with 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  998                │  548                │  357                │  949                │  599                │  357                │  353                │  1497               │
    │ clear with 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  998                │  548                │  357                │  949                │  599                │  357                │  353                │  1497               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 3918                │ 1668                │  677                │ 3669                │ 1919                │  677                │  673                │  6137               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 3918                │ 1668                │  677                │ 3669                │ 1919                │  677                │  673                │  6137               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 7568                │ 3068                │ 1077                │ 7069                │ 3569                │ 1077                │ 1073                │ 11937               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 window           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │  998                │  548                │  357                │  949                │  599                │  357                │  353                │  1497               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │ 7568                │ 3068                │ 1077                │ 7069                │ 3569                │ 1077                │ 1073                │ 11937               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │ 7641                │ 3096                │ 1085                │ 7137                │ 3602                │ 1085                │ 1081                │ 12053               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │ 7641                │ 3096                │ 1085                │ 7137                │ 3602                │ 1085                │ 1081                │ 12053               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 1 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 5 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 1 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 5 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 1 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 5 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 1 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  997                │  547                │  356                │  948                │  598                │  356                │  352                │  1496               │
    │ 5 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │ 7567                │ 3067                │ 1076                │ 7068                │ 3568                │ 1076                │ 1072                │ 11936               │
    │ 10 items in a 1000   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100-wide window      │                     │                     │                     │                     │                     │                     │                     │                     │
    └──────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘

    ====== Nodes Created ======
    ┌──────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┐
    │                      │ new incr_cells      │ new incr_rows       │ new pure (dynamic   │ new incr_cells      │ new incr_rows       │ new pure (static    │ dyn cols            │ dyn cells           │
    │                      │ (dynamic cols)      │ (dynamic cols)      │ cols) (counters)    │ (static cols)       │ (static cols)       │ cols) (counters)    │ (counters) (groups) │ (counters) (groups) │
    │                      │ (counters) (groups) │ (counters) (groups) │ (groups)            │ (counters) (groups) │ (counters) (groups) │ (groups)            │                     │                     │
    ├──────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┤
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 10 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 100 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 101 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 1000 element map  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 10000 element map │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 rows              │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 101 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 1000 rows            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10000 rows           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10000 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  1297               │  257                │  17                 │   897               │  337                │  17                 │  17                 │  1265               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  1617               │  577                │ 209                 │  1217               │  657                │ 209                 │ 209                 │  2225               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  1297               │  257                │  17                 │   897               │  337                │  17                 │  17                 │  1265               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  1617               │  577                │ 209                 │  1217               │  657                │ 209                 │ 209                 │  2225               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 100-wide      │  1617               │  577                │ 209                 │  1217               │  657                │ 209                 │ 209                 │  2225               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3068               │  728                │ 188                 │  2168               │  908                │ 188                 │ 188                 │  2996               │
    │ clear with 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3068               │  728                │ 188                 │  2168               │  908                │ 188                 │ 188                 │  2996               │
    │ clear with 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3068               │  728                │ 188                 │  2168               │  908                │ 188                 │ 188                 │  2996               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 15868               │ 3128                │ 188                 │ 10968               │ 4108                │ 188                 │ 188                 │ 15476               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 15868               │ 3128                │ 188                 │ 10968               │ 4108                │ 188                 │ 188                 │ 15476               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 31868               │ 6128                │ 188                 │ 21968               │ 8108                │ 188                 │ 188                 │ 31076               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 window           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │    49               │   49                │  49                 │    49               │   49                │  49                 │  49                 │    49               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │    49               │   49                │  49                 │    49               │   49                │  49                 │  49                 │    49               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │    49               │   49                │  49                 │    49               │   49                │  49                 │  49                 │    49               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │    49               │   49                │  49                 │    49               │   49                │  49                 │  49                 │    49               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │     0               │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10 items in a 1000   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100-wide window      │                     │                     │                     │                     │                     │                     │                     │                     │
    └──────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘

    ====== Nodes Recomputed ======
    ┌──────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┐
    │                      │ new incr_cells      │ new incr_rows       │ new pure (dynamic   │ new incr_cells      │ new incr_rows       │ new pure (static    │ dyn cols            │ dyn cells           │
    │                      │ (dynamic cols)      │ (dynamic cols)      │ cols) (counters)    │ (static cols)       │ (static cols)       │ cols) (counters)    │ (counters) (groups) │ (counters) (groups) │
    │                      │ (counters) (groups) │ (counters) (groups) │ (groups)            │ (counters) (groups) │ (counters) (groups) │ (groups)            │                     │                     │
    ├──────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┤
    │ Focus by key (key    │    56               │    56               │   52                │    56               │    56               │   52                │   56                │    52               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    56               │    56               │   52                │    56               │    56               │   52                │   56                │    52               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    56               │    56               │   52                │    56               │    56               │   52                │   56                │    52               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    56               │    56               │   52                │    56               │    56               │   52                │   56                │    52               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    56               │    56               │   52                │    56               │    56               │   52                │   56                │    52               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │   126               │   126               │  122                │   126               │   126               │  122                │  126                │   122               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │   306               │   306               │  302                │   306               │   306               │  302                │  306                │   302               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │   308               │   308               │  304                │   308               │   308               │  304                │  308                │   304               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │   308               │   308               │  304                │   308               │   308               │  304                │  308                │   304               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │   308               │   308               │  304                │   308               │   308               │  304                │  308                │   304               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    63               │    63               │   61                │    63               │    63               │   61                │   63                │    61               │
    │ in 10 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │   153               │   153               │  151                │   153               │   153               │  151                │  153                │   151               │
    │ in 100 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ in 101 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ in 1000 element map  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ in 10000 element map │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    63               │    63               │   61                │    63               │    63               │   61                │   63                │    61               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 rows              │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │   153               │   153               │  151                │   153               │   153               │  151                │  153                │   151               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 101 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 1000 rows            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10000 rows           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    63               │    63               │   61                │    63               │    63               │   61                │   63                │    61               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │   153               │   153               │  151                │   153               │   153               │  151                │  153                │   151               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │   154               │   154               │  152                │   154               │   154               │  152                │  154                │   152               │
    │ 10000 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  2497               │  1777               │ 1508                │  2417               │  1857               │ 1508                │ 1508                │  3865               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  2513               │  1793               │ 1524                │  2433               │  1873               │ 1524                │ 1524                │  3881               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  2497               │  1777               │ 1508                │  2417               │  1857               │ 1508                │ 1508                │  3865               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  2513               │  1793               │ 1524                │  2433               │  1873               │ 1524                │ 1524                │  3881               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 100-wide      │  2513               │  1793               │ 1524                │  2433               │  1873               │ 1524                │ 1524                │  3881               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3000               │  1380               │  680                │  2820               │  1560               │  680                │  680                │  4812               │
    │ clear with 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3000               │  1380               │  680                │  2820               │  1560               │  680                │  680                │  4812               │
    │ clear with 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  3000               │  1380               │  680                │  2820               │  1560               │  680                │  680                │  4812               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 14520               │  5700               │ 1800                │ 13540               │  6680               │ 1800                │ 1800                │ 23212               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 14520               │  5700               │ 1800                │ 13540               │  6680               │ 1800                │ 1800                │ 23212               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 28920               │ 11100               │ 3200                │ 26940               │ 13080               │ 3200                │ 3200                │ 46212               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 window           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   131               │   131               │  160                │   131               │   131               │  160                │  160                │   415               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   491               │   491               │  700                │   491               │   491               │  700                │  700                │  3115               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   495               │   495               │  706                │   495               │   495               │  706                │  706                │  3145               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   495               │   495               │  706                │   495               │   495               │  706                │  706                │  3145               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │     0               │     0               │    0                │     0               │     0               │    0                │    0                │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1018               │   923               │  744                │  1113               │  1018               │  744                │  744                │  2140               │
    │ 1 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1810               │  1535               │  960                │  2085               │  1810               │  960                │  960                │  4300               │
    │ 5 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1018               │   923               │  744                │  1113               │  1018               │  744                │  744                │  2140               │
    │ 1 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1810               │  1535               │  960                │  2085               │  1810               │  960                │  960                │  4300               │
    │ 5 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1018               │   923               │  744                │  1113               │  1018               │  744                │  744                │  2140               │
    │ 1 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1810               │  1535               │  960                │  2085               │  1810               │  960                │  960                │  4300               │
    │ 5 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1018               │   923               │  744                │  1113               │  1018               │  744                │  744                │  2140               │
    │ 1 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  1810               │  1535               │  960                │  2085               │  1810               │  960                │  960                │  4300               │
    │ 5 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │  4780               │  3830               │ 1770                │  5730               │  4780               │ 1770                │ 1770                │ 12400               │
    │ 10 items in a 1000   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100-wide window      │                     │                     │                     │                     │                     │                     │                     │                     │
    └──────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘

    ====== Nodes Invalidated ======
    ┌──────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┬─────────────────────┐
    │                      │ new incr_cells      │ new incr_rows       │ new pure (dynamic   │ new incr_cells      │ new incr_rows       │ new pure (static    │ dyn cols            │ dyn cells           │
    │                      │ (dynamic cols)      │ (dynamic cols)      │ cols) (counters)    │ (static cols)       │ (static cols)       │ cols) (counters)    │ (counters) (groups) │ (counters) (groups) │
    │                      │ (counters) (groups) │ (counters) (groups) │ (groups)            │ (counters) (groups) │ (counters) (groups) │ (groups)            │                     │                     │
    ├──────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┼─────────────────────┤
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ not present) and     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10        │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus by key (key    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ present) and         │                     │                     │                     │                     │                     │                     │                     │                     │
    │ unfocus in 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 10 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 100 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 101 element map   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 1000 element map  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus up and down    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ in 10000 element map │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 rows              │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 101 rows             │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 1000 rows            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Focus left and       │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ right in a map with  │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10000 rows           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Page up and down in  │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10000 element map    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  515                │  435                │ 112                 │  1075               │  515                │ 112                 │ 112                 │  1858               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  225                │  225                │ 119                 │   225               │  225                │ 119                 │ 119                 │   755               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 100      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 1-wide        │  517                │  437                │ 113                 │  1077               │  517                │ 113                 │ 113                 │  1865               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 10-wide       │  227                │  227                │ 120                 │   227               │  227                │ 120                 │ 120                 │   762               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Scroll 100-wide      │   47                │   47                │  30                 │    47               │   47                │  30                 │  30                 │   132               │
    │ window from 0 to 9   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ and back in 1000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map          │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  867                │  687                │ 183                 │  2127               │  867                │ 183                 │ 183                 │  2775               │
    │ clear with 100       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  867                │  687                │ 183                 │  2127               │  867                │ 183                 │ 183                 │  2775               │
    │ clear with 101       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │  867                │  687                │ 183                 │  2127               │  867                │ 183                 │ 183                 │  2775               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 3907                │ 2927                │ 183                 │ 10767               │ 3907                │ 183                 │ 183                 │ 14295               │
    │ clear with 1000      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 3907                │ 2927                │ 183                 │ 10767               │ 3907                │ 183                 │ 183                 │ 14295               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 50 window            │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Apply 4 filters and  │ 7707                │ 5727                │ 183                 │ 21567               │ 7707                │ 183                 │ 183                 │ 28695               │
    │ clear with 10000     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map using    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100 window           │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   47                │   47                │  47                 │    47               │   47                │  47                 │  47                 │    47               │
    │ 10 element map       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   47                │   47                │  47                 │    47               │   47                │  47                 │  47                 │    47               │
    │ 100 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   47                │   47                │  47                 │    47               │   47                │  47                 │  47                 │    47               │
    │ 101 element map      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Invert ordering of   │   47                │   47                │  47                 │    47               │   47                │  47                 │  47                 │    47               │
    │ 1000 element map     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ one cell in it.      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Randomly select a    │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ row, then change     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ all cells in it.     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 10      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 11      │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 100     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 1 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 5 items in a 1000    │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 10-wide window       │                     │                     │                     │                     │                     │                     │                     │                     │
    │ Perform 10 sets of   │    0                │    0                │   0                 │     0               │    0                │   0                 │   0                 │     0               │
    │ 10 items in a 1000   │                     │                     │                     │                     │                     │                     │                     │                     │
    │ element map with     │                     │                     │                     │                     │                     │                     │                     │                     │
    │ 100-wide window      │                     │                     │                     │                     │                     │                     │                     │                     │
    └──────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┴─────────────────────┘
    |}]
;;
