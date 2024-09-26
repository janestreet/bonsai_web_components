open! Core

module Tester = Utils.Make_sexp_tester (struct
    type t = char [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "grammar_to_string" =
  Tester.print_grammar ();
  [%expect {| <char> |}]
;;

let%expect_test "quickcheck" = Tester.quickcheck_tests ()

let%expect_test "valid char" =
  Tester.print_test_t "a";
  [%expect {| |}]
;;

let%expect_test "invalid atom for char" =
  Tester.print_test_t "Hello";
  [%expect
    {|
    Expected a single character, like a or 1.
    Parsed: `Hello`.

    1| Hello
       ^^^^^
    |}]
;;

let%expect_test "2 atoms" =
  Tester.print_test_t "5 5";
  [%expect
    {|
    Expected only one S-expression, but parsed the start of a second S-expression.

    1| 5 5
         ^
    |}]
;;

let%expect_test "parenthesized char" =
  Tester.print_test_t "(a)";
  [%expect
    {|
    Expected a char.
    Parsed: `(a)`.
    Maybe you meant `a`?

    1| (a)
       ^^^
    |}]
;;

let%expect_test "list for char" =
  Tester.print_test_t "((a 1) (b 2))";
  [%expect
    {|
    Expected a char.
    Parsed: `((a 1)(b 2))`.

    1| ((a 1) (b 2))
       ^^^^^^^^^^^^^
    |}]
;;
