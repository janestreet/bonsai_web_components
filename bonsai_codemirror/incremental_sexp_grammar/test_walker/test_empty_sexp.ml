open! Core

let print_test_empty =
  Utils.walk_tokens_and_print_errors ~grammar:(Sexp_grammar.List Empty)
;;

let%expect_test "empty list" =
  print_test_empty "()";
  [%expect {| |}]
;;

let%expect_test "invalid atom for list" =
  print_test_empty "(1 2 3)";
  [%expect
    {|
    Expected an empty list.
    Parsed: `(1 2 3)`.

    1| (1 2 3)
       ^^^^^^^
    |}]
;;

let%expect_test "invalid atom for list with newline" =
  print_test_empty "(1 2\n3)";
  [%expect
    {|
    Expected an empty list.
    Parsed: `(1 2 3)`.

    1| (1 2
       ^^^^
    2| 3)
       ^^
    |}]
;;
