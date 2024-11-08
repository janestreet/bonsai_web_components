open! Core
module Tester = Utils.Make_sexp_tester (Sexp)

let%expect_test "grammar_to_string" =
  Tester.print_grammar ();
  [%expect {| <any> |}]
;;

let%expect_test "empty string" =
  Tester.print_test_t "";
  [%expect
    {| No sexp was provided. Sexp string must contain non-whitespace characters. |}]
;;

let%expect_test "just newline" =
  Tester.print_test_t "\n";
  [%expect
    {|
    No sexp was provided. Sexp string must contain non-whitespace characters.

    1|
       ^
    |}]
;;

let%expect_test "just whitespace" =
  Tester.print_test_t "\n  \n ";
  [%expect
    {|
    No sexp was provided. Sexp string must contain non-whitespace characters.

    1|
       ^
    |}]
;;
