open! Core
open Codemirror_incremental_sexp_grammar.Correction

let test corrections =
  match list_to_string corrections with
  | None -> ()
  | Some s -> print_endline s
;;

let%expect_test "no corrections" =
  test [];
  [%expect {| |}]
;;

let%expect_test "no kind" =
  test [ other "a" ];
  [%expect {| Maybe you meant `a`? |}];
  test [ other "a"; other "b" ];
  [%expect {| Maybe you meant `a` or `b`? |}];
  test [ other "a"; other "b"; other "c" ];
  [%expect {| Maybe you meant `a`, `b` or `c`? |}];
  test [ other "a"; other "b"; other "c"; other "d" ];
  [%expect {| Maybe you meant `a`, `b`, `c` or `d`? |}]
;;

let%expect_test "all capitalization" =
  test [ capitalization "a" ];
  [%expect {| Incorrect capitalization; maybe you meant `a`? |}];
  test [ capitalization "a"; capitalization "b" ];
  [%expect {| Incorrect capitalization; maybe you meant `a` or `b`? |}];
  test [ capitalization "a"; capitalization "b"; capitalization "c" ];
  [%expect {| Incorrect capitalization; maybe you meant `a`, `b` or `c`? |}];
  test [ capitalization "a"; capitalization "b"; capitalization "c"; capitalization "d" ];
  [%expect {| Incorrect capitalization; maybe you meant `a`, `b`, `c` or `d`? |}]
;;

let%expect_test "spelling mistake" =
  test [ spelling_mistake "a" ];
  [%expect {| Possible spelling mistake; maybe you meant `a`? |}];
  test [ spelling_mistake "a"; spelling_mistake "b" ];
  [%expect {| Possible spelling mistake; maybe you meant `a` or `b`? |}];
  test [ spelling_mistake "a"; spelling_mistake "b"; spelling_mistake "c" ];
  [%expect {| Possible spelling mistake; maybe you meant `a`, `b` or `c`? |}];
  test
    [ spelling_mistake "a"
    ; spelling_mistake "b"
    ; spelling_mistake "c"
    ; spelling_mistake "d"
    ];
  [%expect {| Possible spelling mistake; maybe you meant `a`, `b`, `c` or `d`? |}]
;;

let%expect_test "combos of 2" =
  test [ capitalization "a"; spelling_mistake "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}];
  test [ spelling_mistake "a"; capitalization "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}];
  test [ other "a"; spelling_mistake "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}];
  test [ spelling_mistake "a"; other "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}];
  test [ capitalization "a"; other "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}];
  test [ other "a"; capitalization "b" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a` or `b`? |}]
;;

let%expect_test "combos of c" =
  test [ other "a"; capitalization "b"; spelling_mistake "c" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a`, `b` or `c`? |}];
  test [ other "a"; other "b"; spelling_mistake "c" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a`, `b` or `c`? |}];
  test [ capitalization "a"; capitalization "b"; spelling_mistake "c" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a`, `b` or `c`? |}];
  test [ other "a"; spelling_mistake "b"; spelling_mistake "c" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a`, `b` or `c`? |}];
  test [ capitalization "a"; spelling_mistake "b"; spelling_mistake "c" ];
  [%expect
    {| Possible incorrect capitalization or spelling; maybe you meant `a`, `b` or `c`? |}]
;;
