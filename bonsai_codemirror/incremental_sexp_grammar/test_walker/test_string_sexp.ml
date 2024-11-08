open Core

module Tester = Utils.Make_sexp_tester (struct
    type t = string [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "grammar_to_string" =
  Tester.print_grammar ();
  [%expect {| <string> |}]
;;

let%expect_test "quickcheck" = Tester.quickcheck_tests ()

let%expect_test "valid string" =
  Tester.print_test_t "hello";
  [%expect {| |}]
;;

let%expect_test "valid string - empty" =
  Tester.print_test_t "\"\"";
  [%expect {| |}]
;;

let%expect_test "parenthesized string" =
  Tester.print_test_t "(hello)";
  [%expect
    {|
    Expected a string.
    Parsed: `(hello)`.
    Maybe you meant `hello`?

    1| (hello)
       ^^^^^^^
    |}]
;;

let%expect_test "list of strings" =
  Tester.print_test_t "(hello there, world!)";
  [%expect
    {|
    Expected a string.
    Parsed: `(hello there, world!)`.

    1| (hello there, world!)
       ^^^^^^^^^^^^^^^^^^^^^
    |}]
;;

let%expect_test "list for string" =
  Tester.print_test_t "((a 1) (b 2))";
  [%expect
    {|
    Expected a string.
    Parsed: `((a 1)(b 2))`.

    1| ((a 1) (b 2))
       ^^^^^^^^^^^^^
    |}]
;;
