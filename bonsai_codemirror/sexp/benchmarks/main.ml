open! Core
open Codemirror_sexp_kernel
module Doc = Codemirror_sexp_for_testing.Doc

module Rose_tree = struct
  type 'a t =
    | Leaf of 'a
    | Node of 'a t list
  [@@deriving sexp_of, sexp_grammar]

  let rec generate ~depth ~width =
    match depth with
    | 0 -> Leaf "test(();;#0"
    | _ -> List.init width ~f:(fun _ -> generate ~depth:(depth - 1) ~width) |> Node
  ;;

  let pp_tree t = sexp_of_t String.sexp_of_t t |> Sexp.to_string_hum
end

let%expect_test "test that generated strings look right" =
  Rose_tree.generate ~depth:2 ~width:2 |> Rose_tree.pp_tree |> print_endline;
  [%expect
    {|
    (Node
     ((Node ((Leaf "test(();;#0") (Leaf "test(();;#0")))
      (Node ((Leaf "test(();;#0") (Leaf "test(();;#0")))))
    |}]
;;

(* Original highlight function (highlighting full text):
   ┌────────────────────────────────────────────────┬──────────────────┬────────────────┬───────────────┬───────────────┬────────────┐
   │ Name                                           │         Time/Run │        mWd/Run │      mjWd/Run │      Prom/Run │ Percentage │
   ├────────────────────────────────────────────────┼──────────────────┼────────────────┼───────────────┼───────────────┼────────────┤
   │ [test_highlight.ml] shallow text highlight:1   │         757.84ns │        891.00w │               │               │            │
   │ [test_highlight.ml] shallow text highlight:4   │       8_274.36ns │      9_897.00w │         1.59w │         1.59w │            │
   │ [test_highlight.ml] shallow text highlight:16  │     119_977.25ns │    143_553.00w │       243.13w │       243.13w │      0.10% │
   │ [test_highlight.ml] shallow text highlight:64  │   2_413_536.35ns │  2_244_897.00w │    39_320.83w │    39_320.83w │      2.10% │
   │ [test_highlight.ml] shallow text highlight:256 │  39_730_228.99ns │ 35_717_793.00w │   782_739.15w │   782_739.15w │     34.60% │
   │ [test_highlight.ml] deep text highlight:1      │         995.56ns │      1_219.00w │               │               │            │
   │ [test_highlight.ml] deep text highlight:4      │      10_977.17ns │     12_285.00w │         3.30w │         3.30w │            │
   │ [test_highlight.ml] deep text highlight:16     │ 114_838_497.91ns │ 97_386_381.00w │ 2_350_336.57w │ 2_350_336.57w │    100.00% │
   └────────────────────────────────────────────────┴──────────────────┴────────────────┴───────────────┴───────────────┴────────────┘

   New highlight function:
   ┌──────────────────────────────────────────────────────────────┬──────────────────┬─────────────────┬───────────────┬───────────────┬────────────┐
   │ Name                                                         │         Time/Run │         mWd/Run │      mjWd/Run │      Prom/Run │ Percentage │
   ├──────────────────────────────────────────────────────────────┼──────────────────┼─────────────────┼───────────────┼───────────────┼────────────┤
   │ [test_highlight.ml] shallow text full highlight:1            │       1_373.28ns │       2_294.00w │         0.14w │         0.14w │            │
   │ [test_highlight.ml] shallow text full highlight:4            │      14_071.07ns │      23_137.00w │         4.27w │         4.27w │            │
   │ [test_highlight.ml] shallow text full highlight:16           │     205_629.84ns │     331_022.00w │       579.40w │       579.40w │      0.13% │
   │ [test_highlight.ml] shallow text full highlight:64           │   3_783_886.30ns │   5_164_488.00w │    47_231.58w │    47_231.58w │      2.36% │
   │ [test_highlight.ml] shallow text full highlight:256          │  59_772_924.28ns │  82_144_120.00w │   815_414.48w │   815_414.48w │     37.30% │
   │ [test_highlight.ml] deep text full highlight:1               │       1_802.54ns │       2_967.00w │         0.18w │         0.18w │            │
   │ [test_highlight.ml] deep text full highlight:4               │      17_874.08ns │      29_331.00w │         8.64w │         8.64w │      0.01% │
   │ [test_highlight.ml] deep text full highlight:16              │ 160_256_415.42ns │ 210_021_887.00w │ 2_510_031.53w │ 2_510_031.53w │    100.00% │
   │ [test_highlight.ml] shallow text partial highlight:0         │         255.04ns │         138.00w │               │               │            │
   │ [test_highlight.ml] shallow text partial highlight:100       │     234_222.80ns │     369_816.00w │     1_508.57w │       681.57w │      0.15% │
   │ [test_highlight.ml] shallow text partial highlight:200       │     484_536.39ns │     739_499.00w │     4_309.46w │     2_657.46w │      0.30% │
   │ [test_highlight.ml] shallow text partial highlight:400       │   1_028_486.99ns │   1_478_862.00w │    12_919.90w │     9_617.90w │      0.64% │
   │ [test_highlight.ml] shallow text partial highlight:1000      │   2_671_268.52ns │   3_691_360.00w │    39_983.86w │    31_742.86w │      1.67% │
   │ [test_highlight.ml] shallow text partial highlight:5000      │  13_634_506.42ns │  18_445_074.00w │   220_434.53w │   179_263.53w │      8.51% │
   │ [test_highlight.ml] shallow text partial highlight:10000     │  27_354_890.38ns │  36_884_333.00w │   446_086.39w │   363_757.39w │     17.07% │
   │ [test_highlight.ml] shallow text partial highlight:20000     │  53_731_867.62ns │  73_761_254.00w │   732_338.78w │   732_338.78w │     33.53% │
   │ [test_highlight.ml] deep text partial highlight:0            │         254.25ns │         138.00w │               │               │            │
   │ [test_highlight.ml] deep text partial highlight:100          │     135_140.05ns │     212_322.00w │       877.68w │       273.68w │      0.08% │
   │ [test_highlight.ml] deep text partial highlight:200          │     280_936.23ns │     427_838.00w │     2_295.68w │     1_077.68w │      0.18% │
   │ [test_highlight.ml] deep text partial highlight:400          │     588_222.24ns │     856_278.00w │     6_679.30w │     4_241.30w │      0.37% │
   │ [test_highlight.ml] deep text partial highlight:1000         │   1_586_319.57ns │   2_135_302.00w │    25_634.38w │    19_552.38w │      0.99% │
   │ [test_highlight.ml] deep text partial highlight:5000         │   8_222_348.40ns │  10_681_376.00w │   152_313.15w │   121_893.15w │      5.13% │
   │ [test_highlight.ml] deep text partial highlight:10000        │  16_642_653.88ns │  21_362_808.00w │   310_829.63w │   249_988.63w │     10.39% │
   │ [test_highlight.ml] deep text partial highlight:20000        │  32_568_140.78ns │  42_703_611.00w │   504_536.63w │   504_536.63w │     20.32% │
   │ [test_highlight.ml] shallow viewport highlight at line:0     │     234_661.12ns │     369_578.00w │     1_501.19w │       674.19w │      0.15% │
   │ [test_highlight.ml] shallow viewport highlight at line:5000  │     234_347.95ns │     369_760.00w │     1_508.19w │       681.19w │      0.15% │
   │ [test_highlight.ml] shallow viewport highlight at line:10000 │     234_026.91ns │     369_782.00w │     1_502.93w │       675.93w │      0.15% │
   │ [test_highlight.ml] shallow viewport highlight at line:15000 │     234_213.62ns │     369_816.00w │     1_506.10w │       679.10w │      0.15% │
   │ [test_highlight.ml] shallow viewport highlight at line:20000 │     233_504.56ns │     369_816.00w │     1_503.25w │       676.25w │      0.15% │
   │ [test_highlight.ml] deep viewport highlight at line:0        │     137_880.52ns │     216_081.00w │       894.91w │       278.91w │      0.09% │
   │ [test_highlight.ml] deep viewport highlight at line:20000    │     135_757.62ns │     213_103.00w │       881.26w │       272.26w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:40000    │     135_784.74ns │     213_091.00w │       882.70w │       273.70w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:60000    │     133_095.25ns │     209_772.00w │       827.89w │       227.89w │      0.08% │
   │ [test_highlight.ml] deep viewport highlight at line:80000    │     136_960.83ns │     215_717.00w │       893.95w │       278.95w │      0.09% │
   └──────────────────────────────────────────────────────────────┴──────────────────┴─────────────────┴───────────────┴───────────────┴────────────┘

   Grammared / Ungrammared Implementation:
    ┌───────────────────────────────────────────────────┬──────────────────┬──────────────┬───────────────┬───────────────┬────────────┐
    │ Name                                              │         Time/Run │      mWd/Run │      mjWd/Run │      Prom/Run │ Percentage │
    ├───────────────────────────────────────────────────┼──────────────────┼──────────────┼───────────────┼───────────────┼────────────┤
    │ [main.ml] shallow text full tokenize:1            │       1_423.45ns │       3.18kw │         0.83w │         0.83w │            │
    │ [main.ml] shallow text full tokenize:4            │      17_284.20ns │      32.42kw │        68.55w │        68.55w │            │
    │ [main.ml] shallow text full tokenize:16           │     366_858.78ns │     462.41kw │    10_915.22w │    10_915.22w │      0.13% │
    │ [main.ml] shallow text full tokenize:64           │   6_267_467.08ns │   7_206.66kw │   227_239.95w │   227_239.95w │      2.14% │
    │ [main.ml] shallow text full tokenize:256          │  96_487_929.47ns │ 114_585.92kw │ 3_695_806.09w │ 3_695_806.09w │     33.01% │
    │ [main.ml] deep text full tokenize:1               │       1_864.01ns │       4.13kw │         1.28w │         1.28w │            │
    │ [main.ml] deep text full tokenize:4               │      22_457.95ns │      41.07kw │       114.47w │       114.47w │            │
    │ [main.ml] deep text full tokenize:16              │ 225_957_870.21ns │ 315_774.97kw │ 6_805_114.80w │ 6_805_114.80w │     77.30% │
    │ [main.ml] shallow text partial tokenize:0         │           3.81ns │              │               │               │            │
    │ [main.ml] shallow text partial tokenize:100       │     439_533.65ns │     515.66kw │    16_815.47w │    16_815.47w │      0.15% │
    │ [main.ml] shallow text partial tokenize:200       │     866_441.21ns │   1_031.33kw │    33_878.13w │    33_878.13w │      0.30% │
    │ [main.ml] shallow text partial tokenize:400       │   1_735_963.24ns │   2_062.66kw │    68_389.18w │    68_389.18w │      0.59% │
    │ [main.ml] shallow text partial tokenize:1000      │   4_326_521.42ns │   5_148.87kw │   168_315.32w │   168_315.32w │      1.48% │
    │ [main.ml] shallow text partial tokenize:5000      │  21_655_042.98ns │  25_728.80kw │   843_420.21w │   843_420.21w │      7.41% │
    │ [main.ml] shallow text partial tokenize:10000     │  43_209_729.79ns │  51_449.30kw │ 1_666_103.79w │ 1_666_103.79w │     14.78% │
    │ [main.ml] shallow text partial tokenize:20000     │  86_508_482.07ns │ 102_893.27kw │ 3_311_395.40w │ 3_311_395.40w │     29.59% │
    │ [main.ml] deep text partial tokenize:0            │           3.82ns │              │               │               │            │
    │ [main.ml] deep text partial tokenize:100          │     226_255.69ns │     318.70kw │     6_833.74w │     6_833.74w │      0.08% │
    │ [main.ml] deep text partial tokenize:200          │     457_109.30ns │     642.73kw │    13_862.07w │    13_862.07w │      0.16% │
    │ [main.ml] deep text partial tokenize:400          │     915_162.03ns │   1_287.06kw │    28_069.53w │    28_069.53w │      0.31% │
    │ [main.ml] deep text partial tokenize:1000         │   2_284_122.41ns │   3_210.60kw │    69_247.20w │    69_247.20w │      0.78% │
    │ [main.ml] deep text partial tokenize:5000         │  11_457_621.35ns │  16_061.56kw │   345_980.09w │   345_980.09w │      3.92% │
    │ [main.ml] deep text partial tokenize:10000        │  23_020_474.08ns │  32_123.26kw │   712_754.33w │   712_754.33w │      7.87% │
    │ [main.ml] deep text partial tokenize:20000        │  45_554_848.96ns │  64_219.59kw │ 1_370_860.07w │ 1_370_860.07w │     15.58% │
    │ [main.ml] shallow viewport tokenize at line:0     │     863_160.08ns │   1_031.05kw │    33_671.30w │    33_671.30w │      0.30% │
    │ [main.ml] shallow viewport tokenize at line:5000  │     863_089.25ns │   1_031.26kw │    34_006.36w │    34_006.36w │      0.30% │
    │ [main.ml] shallow viewport tokenize at line:10000 │     872_273.01ns │   1_031.29kw │    34_124.03w │    34_124.03w │      0.30% │
    │ [main.ml] shallow viewport tokenize at line:15000 │     870_282.11ns │   1_031.33kw │    33_763.03w │    33_763.03w │      0.30% │
    │ [main.ml] shallow viewport tokenize at line:20000 │     871_859.30ns │   1_031.33kw │    33_863.26w │    33_863.26w │      0.30% │
    │ [main.ml] deep viewport tokenize at line:0        │     452_033.37ns │     640.07kw │    13_827.20w │    13_827.20w │      0.15% │
    │ [main.ml] deep viewport tokenize at line:20000    │     469_202.91ns │     644.37kw │    16_433.26w │    16_433.26w │      0.16% │
    │ [main.ml] deep viewport tokenize at line:40000    │     469_300.13ns │     644.31kw │    15_389.67w │    15_389.67w │      0.16% │
    │ [main.ml] deep viewport tokenize at line:60000    │     448_144.83ns │     635.18kw │    13_825.95w │    13_825.95w │      0.15% │
    │ [main.ml] deep viewport tokenize at line:80000    │     456_377.62ns │     643.94kw │    13_739.28w │    13_739.28w │      0.16% │
    │ [main.ml] shallow text full parse:1               │       2_396.97ns │       5.11kw │         1.35w │         1.35w │            │
    │ [main.ml] shallow text full parse:4               │      24_401.45ns │      46.96kw │        87.07w │        87.07w │            │
    │ [main.ml] shallow text full parse:16              │     476_559.03ns │     652.26kw │    12_654.47w │    12_654.47w │      0.16% │
    │ [main.ml] shallow text full parse:64              │   8_404_422.52ns │  10_096.59kw │   265_300.17w │   265_300.17w │      2.87% │
    │ [main.ml] shallow text full parse:256             │ 125_653_163.43ns │ 160_257.26kw │ 4_300_792.50w │ 4_300_792.50w │     42.98% │
    │ [main.ml] deep text full parse:1                  │       2_890.70ns │       6.05kw │         1.75w │         1.75w │            │
    │ [main.ml] deep text full parse:4                  │      33_137.48ns │      62.73kw │       150.87w │       150.87w │      0.01% │
    │ [main.ml] deep text full parse:16                 │ 292_329_788.00ns │ 408_124.13kw │ 7_662_246.30w │ 7_662_246.30w │    100.00% │
    │ [main.ml] shallow text partial parse:0            │           4.11ns │              │               │               │            │
    │ [main.ml] shallow text partial parse:100          │     559_926.89ns │     721.13kw │    19_316.51w │    19_316.51w │      0.19% │
    │ [main.ml] shallow text partial parse:200          │   1_124_226.39ns │   1_442.27kw │    38_579.89w │    38_579.89w │      0.38% │
    │ [main.ml] shallow text partial parse:400          │   2_256_114.34ns │   2_884.53kw │    75_730.44w │    75_730.44w │      0.77% │
    │ [main.ml] shallow text partial parse:1000         │   5_608_230.23ns │   7_200.82kw │   193_131.39w │   193_131.39w │      1.92% │
    │ [main.ml] shallow text partial parse:5000         │  28_112_493.22ns │  35_983.06kw │   958_800.57w │   958_800.57w │      9.62% │
    │ [main.ml] shallow text partial parse:10000        │  56_712_878.78ns │  71_955.08kw │ 1_935_124.79w │ 1_935_124.79w │     19.40% │
    │ [main.ml] shallow text partial parse:20000        │ 114_373_008.20ns │ 143_904.13kw │ 3_895_315.40w │ 3_895_315.40w │     39.12% │
    │ [main.ml] deep text partial parse:0               │           4.07ns │              │               │               │            │
    │ [main.ml] deep text partial parse:100             │     289_021.68ns │     413.39kw │     7_456.05w │     7_456.05w │      0.10% │
    │ [main.ml] deep text partial parse:200             │     569_393.60ns │     831.84kw │    16_024.00w │    16_024.00w │      0.19% │
    │ [main.ml] deep text partial parse:400             │   1_149_252.85ns │   1_664.06kw │    31_736.69w │    31_736.69w │      0.39% │
    │ [main.ml] deep text partial parse:1000            │   2_865_904.60ns │   4_150.52kw │    77_325.80w │    77_325.80w │      0.98% │
    │ [main.ml] deep text partial parse:5000            │  14_243_890.62ns │  20_759.69kw │   398_204.07w │   398_204.07w │      4.87% │
    │ [main.ml] deep text partial parse:10000           │  28_535_403.38ns │  41_518.55kw │   787_860.35w │   787_860.35w │      9.76% │
    │ [main.ml] deep text partial parse:20000           │  56_845_491.95ns │  83_003.25kw │ 1_577_835.75w │ 1_577_835.75w │     19.45% │
    │ [main.ml] shallow viewport parse at line:0        │   1_117_096.92ns │   1_441.99kw │    38_978.74w │    38_978.74w │      0.38% │
    │ [main.ml] shallow viewport parse at line:5000     │   1_118_111.24ns │   1_442.19kw │    37_971.51w │    37_971.51w │      0.38% │
    │ [main.ml] shallow viewport parse at line:10000    │   1_122_902.65ns │   1_442.23kw │    38_409.13w │    38_409.13w │      0.38% │
    │ [main.ml] shallow viewport parse at line:15000    │   1_137_003.03ns │   1_442.27kw │    38_122.38w │    38_122.38w │      0.39% │
    │ [main.ml] shallow viewport parse at line:20000    │   1_120_461.08ns │   1_442.27kw │    38_737.82w │    38_737.82w │      0.38% │
    │ [main.ml] deep viewport parse at line:0           │     554_046.92ns │     827.46kw │    15_050.48w │    15_050.48w │      0.19% │
    │ [main.ml] deep viewport parse at line:20000       │     561_333.02ns │     832.47kw │    14_829.75w │    14_829.75w │      0.19% │
    │ [main.ml] deep viewport parse at line:40000       │     574_859.81ns │     831.95kw │    15_775.60w │    15_775.60w │      0.20% │
    │ [main.ml] deep viewport parse at line:60000       │     558_999.24ns │     820.42kw │    14_716.79w │    14_716.79w │      0.19% │
    │ [main.ml] deep viewport parse at line:80000       │     578_876.12ns │     832.28kw │    15_455.72w │    15_455.72w │      0.20% │
    └───────────────────────────────────────────────────┴──────────────────┴──────────────┴───────────────┴───────────────┴────────────┘
*)

let empty_tokenize = Parsing.Ungrammared.create ~error_conv:Fn.id (module Doc)

let tokenize ?(state = empty_tokenize) ?(max_iterations = 1_000_000) lines () =
  let state_ref = ref state in
  let rec loop = function
    | 0 -> ()
    | _ when Parsing.Ungrammared.is_done !state_ref -> ()
    | n ->
      state_ref := Parsing.Ungrammared.apply_action !state_ref (Parse_some_lines lines);
      loop (n - 1)
  in
  loop max_iterations;
  !state_ref
;;

let tokenize_partial doc ~start_at ~num_to_parse =
  let res = tokenize ~max_iterations:(start_at / 100) doc () in
  tokenize ~state:res ~max_iterations:(num_to_parse / 100) doc
;;

let%bench_fun ("shallow text full tokenize" [@indexed width = [ 1; 4; 16; 64; 256 ]]) =
  Rose_tree.generate ~depth:2 ~width |> Rose_tree.pp_tree |> Doc.create |> tokenize
;;

let%bench_fun ("deep text full tokenize" [@indexed depth = [ 1; 4; 16 ]]) =
  Rose_tree.generate ~depth ~width:2 |> Rose_tree.pp_tree |> Doc.create |> tokenize
;;

(* These benchmarks simulate decorating a fixed 200-line viewport with variable
   amounts of existing checkpoints (e.g. no checkpoints, checkpoints through the
   middle of the viewport, checkpoints past the viewport, etc). *)
let%bench_fun ("shallow text partial tokenize" [@indexed
                                                 num_lines_to_reparse
                                                 = [ 0
                                                   ; 100
                                                   ; 200
                                                   ; 400
                                                   ; 1000
                                                   ; 5000
                                                   ; 10000
                                                   ; 20000
                                                   ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> Doc.create
  |> tokenize_partial
       ~start_at:(20000 - num_lines_to_reparse)
       ~num_to_parse:num_lines_to_reparse
;;

let%bench_fun ("deep text partial tokenize" [@indexed
                                              num_lines_to_reparse
                                              = [ 0
                                                ; 100
                                                ; 200
                                                ; 400
                                                ; 1000
                                                ; 5000
                                                ; 10000
                                                ; 20000
                                                ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> Doc.create
  |> tokenize_partial
       ~start_at:(20000 - num_lines_to_reparse)
       ~num_to_parse:num_lines_to_reparse
;;

(* These benchmarks simulate editing in the viewport at various line locations
   in the text. It's assumed that you've generated checkpoints through the middle of the
   viewport. *)
let%bench_fun ("shallow viewport tokenize at line" [@indexed
                                                     viewport_start
                                                     = [ 0; 5000; 10000; 15000; 20000 ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> Doc.create
  |> tokenize_partial ~start_at:(viewport_start + 100) ~num_to_parse:200
;;

let%bench_fun ("deep viewport tokenize at line" [@indexed
                                                  viewport_start
                                                  = [ 0; 20000; 40000; 60000; 80000 ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> Doc.create
  |> tokenize_partial ~start_at:(viewport_start + 100) ~num_to_parse:200
;;

let empty_parse =
  Parsing.Grammared.create
    ~error_conv:Fn.id
    (module Doc)
    (Rose_tree.t_sexp_grammar string_sexp_grammar).untyped
;;

let parse ?(state = empty_parse) ?(max_iterations = 1_000_000) lines () =
  let state_ref = ref state in
  let rec loop = function
    | 0 -> ()
    | _ when Parsing.Grammared.is_done !state_ref -> ()
    | n ->
      state_ref := Parsing.Grammared.apply_action !state_ref (Parse_some_lines lines);
      loop (n - 1)
  in
  loop max_iterations;
  !state_ref
;;

let parse_partial test_string ~start_at ~num_to_parse =
  let res = parse ~max_iterations:(start_at / 100) test_string () in
  parse ~state:res ~max_iterations:(num_to_parse / 100) test_string
;;

let%bench_fun ("shallow text full parse" [@indexed width = [ 1; 4; 16; 64; 256 ]]) =
  Rose_tree.generate ~depth:2 ~width |> Rose_tree.pp_tree |> Doc.create |> parse
;;

let%bench_fun ("deep text full parse" [@indexed depth = [ 1; 4; 16 ]]) =
  Rose_tree.generate ~depth ~width:2 |> Rose_tree.pp_tree |> Doc.create |> parse
;;

(* These benchmarks simulate decorating a fixed 200-line viewport with variable
   amounts of existing checkpoints (e.g. no checkpoints, checkpoints through the
   middle of the viewport, checkpoints past the viewport, etc). *)
let%bench_fun ("shallow text partial parse" [@indexed
                                              num_lines_to_reparse
                                              = [ 0
                                                ; 100
                                                ; 200
                                                ; 400
                                                ; 1000
                                                ; 5000
                                                ; 10000
                                                ; 20000
                                                ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> Doc.create
  |> parse_partial
       ~start_at:(20000 - num_lines_to_reparse)
       ~num_to_parse:num_lines_to_reparse
;;

let%bench_fun ("deep text partial parse" [@indexed
                                           num_lines_to_reparse
                                           = [ 0
                                             ; 100
                                             ; 200
                                             ; 400
                                             ; 1000
                                             ; 5000
                                             ; 10000
                                             ; 20000
                                             ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> Doc.create
  |> parse_partial
       ~start_at:(20000 - num_lines_to_reparse)
       ~num_to_parse:num_lines_to_reparse
;;

(* These benchmarks simulate editing in the viewport at various line locations
   in the text. It's assumed that you've generated checkpoints through the middle of the
   viewport. *)
let%bench_fun ("shallow viewport parse at line" [@indexed
                                                  viewport_start
                                                  = [ 0; 5000; 10000; 15000; 20000 ]])
  =
  Rose_tree.generate ~depth:2 ~width:256
  |> Rose_tree.pp_tree
  |> Doc.create
  |> parse_partial ~start_at:(viewport_start + 100) ~num_to_parse:200
;;

let%bench_fun ("deep viewport parse at line" [@indexed
                                               viewport_start
                                               = [ 0; 20000; 40000; 60000; 80000 ]])
  =
  Rose_tree.generate ~depth:16 ~width:2
  |> Rose_tree.pp_tree
  |> Doc.create
  |> parse_partial ~start_at:(viewport_start + 100) ~num_to_parse:200
;;
