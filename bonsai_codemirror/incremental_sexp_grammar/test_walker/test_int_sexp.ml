open Core

module Int_tester = Utils.Make_sexp_tester (struct
    type t = int [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "quickcheck" = Int_tester.quickcheck_tests ()

let%expect_test "valid int" =
  Int_tester.print_test_t "-3";
  [%expect {| |}]
;;

let%expect_test "invalid int" =
  Int_tester.print_test_t "hello";
  [%expect
    {|
    Expected an integer value, like -3 or 10.
    Parsed: `hello`.

    1| hello
       ^^^^^
    |}]
;;

let%expect_test "invalid int with commas" =
  Int_tester.print_test_t "1,000,000";
  [%expect
    {|
    Numerical values cannot contain commas.
    Parsed: `1,000,000`.
    Maybe you meant `1000000`?

    1| 1,000,000
       ^^^^^^^^^
    |}]
;;

let%expect_test "invalid int float" =
  Int_tester.print_test_t "3.0";
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.0`.
    Maybe you meant `3`?

    1| 3.0
       ^^^
    |}]
;;

let%expect_test "invalid multiple ints" =
  Int_tester.print_test_t "3 3 3 (3)";
  [%expect
    {|
    Expected only one S-expression, but parsed the start of a second S-expression.

    1| 3 3 3 (3)
         ^
    |}]
;;

let%expect_test "parenthesized int" =
  Int_tester.print_test_t "(3)";
  [%expect
    {|
    Expected an int.
    Parsed: `(3)`.
    Maybe you meant `3`?

    1| (3)
       ^^^
    |}]
;;

let%expect_test "list for int" =
  Int_tester.print_test_t "((a 1) (b 2))";
  [%expect
    {|
    Expected an int.
    Parsed: `((a 1)(b 2))`.

    1| ((a 1) (b 2))
       ^^^^^^^^^^^^^
    |}]
;;
