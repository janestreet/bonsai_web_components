open! Core

module Tester = Utils.Make_sexp_tester (struct
    type t = bool [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "grammar_to_string" =
  Tester.print_grammar ();
  [%expect {| <bool> |}]
;;

let%expect_test "quickcheck" = Tester.quickcheck_tests ()

let%expect_test "valid boolean" =
  Tester.print_test_t "False";
  [%expect {| |}]
;;

let%expect_test "invalid atom for boolean" =
  Tester.print_test_t "Hello";
  [%expect
    {|
    Bool values must be true or false.
    Parsed: `Hello`.

    1| Hello
       ^^^^^
    |}]
;;

let%expect_test "misspelled boolean" =
  Tester.print_test_t "fales";
  [%expect
    {|
    Bool values must be true or false.
    Parsed: `fales`.
    Possible spelling mistake; maybe you meant `false`?

    1| fales
       ^^^^^
    |}]
;;

let%expect_test "capitalized boolean" =
  Tester.print_test_t "FALSE";
  [%expect
    {|
    Bool values must be true or false.
    Parsed: `FALSE`.
    Incorrect capitalization; maybe you meant `false`?

    1| FALSE
       ^^^^^
    |}]
;;

let%expect_test "binary boolean" =
  Tester.print_test_t "1";
  [%expect
    {|
    Bool values must be true or false, not binary.
    Parsed: `1`.
    Maybe you meant `true`?

    1| 1
       ^
    |}]
;;

let%expect_test "parenthesized boolean" =
  Tester.print_test_t "(false)";
  [%expect
    {|
    Expected a bool.
    Parsed: `(false)`.
    Maybe you meant `false`?

    1| (false)
       ^^^^^^^
    |}]
;;

let%expect_test "list for boolean" =
  Tester.print_test_t "((a 1) (b 2))";
  [%expect
    {|
    Expected a bool.
    Parsed: `((a 1)(b 2))`.

    1| ((a 1) (b 2))
       ^^^^^^^^^^^^^
    |}]
;;
