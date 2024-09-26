open! Core

module Tester = Utils.Make_sexp_tester (struct
    type t = float [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "grammar_to_string" =
  Tester.print_grammar ();
  [%expect {| <float> |}]
;;

let%expect_test "quickcheck" = Tester.quickcheck_tests ()

let%expect_test "valid float" =
  Tester.print_test_t "3.14";
  [%expect {| |}]
;;

let%expect_test "valid float (integer)" =
  Tester.print_test_t "3";
  [%expect {| |}]
;;

let%expect_test "invalid atom for float" =
  Tester.print_test_t "Hello";
  [%expect
    {|
    Expected a float value, like -2.0 or 3.14.
    Parsed: `Hello`.

    1| Hello
       ^^^^^
    |}]
;;

let%expect_test "float with commas" =
  Tester.print_test_t "1,000,000.0";
  [%expect
    {|
    Numerical values cannot contain commas.
    Parsed: `1,000,000.0`.
    Maybe you meant `1000000.0`?

    1| 1,000,000.0
       ^^^^^^^^^^^
    |}]
;;

let%expect_test "parenthesized float" =
  Tester.print_test_t "(3.14)";
  [%expect
    {|
    Expected a float.
    Parsed: `(3.14)`.
    Maybe you meant `3.14`?

    1| (3.14)
       ^^^^^^
    |}]
;;

let%expect_test "list for float" =
  Tester.print_test_t "((a 1) (b 2))";
  [%expect
    {|
    Expected a float.
    Parsed: `((a 1)(b 2))`.

    1| ((a 1) (b 2))
       ^^^^^^^^^^^^^
    |}]
;;
