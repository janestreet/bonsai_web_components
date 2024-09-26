open! Core

let%test_module "multiple sexps" =
  (module struct
    module Tester = Utils.Make_sexp_tester (struct
        type t = string [@@deriving sexp, sexp_grammar, quickcheck]
      end)

    let%expect_test "grammar_to_string" =
      Tester.print_grammar ();
      [%expect {| <string> |}]
    ;;

    let%expect_test "quickcheck" = Tester.quickcheck_tests ()

    let%expect_test "valid atom" =
      Tester.print_test_t {|hello|};
      [%expect {| |}]
    ;;

    let%expect_test "multiple atoms" =
      Tester.print_test_t {|foo bar baz|};
      [%expect
        {|
        Expected only one S-expression, but parsed the start of a second S-expression.

        1| foo bar baz
               ^
        |}]
    ;;

    let%expect_test "multiple lists" =
      Tester.print_test_t {|(foo bar) (baz)|};
      [%expect
        {|
        Expected a string.
        Parsed: `(foo bar)`.

        1| (foo bar) (baz)
           ^^^^^^^^^
        Expected only one S-expression, but parsed the start of a second S-expression.

        1| (foo bar) (baz)
                     ^
        |}]
    ;;
  end)
;;

let%test_module "record of strings" =
  (module struct
    module Tester = Utils.Make_sexp_tester (struct
        type t =
          { a : string
          ; b : string
          ; c : string
          }
        [@@deriving sexp, sexp_grammar, quickcheck]
      end)

    let%expect_test "grammar_to_string" =
      Tester.print_grammar ();
      [%expect {| ((a <string>)(b <string>)(c <string>)) |}]
    ;;

    let%expect_test "quickcheck" = Tester.quickcheck_tests ()

    let%expect_test "unmatched open parenthesis" =
      Tester.print_test_t
        {|
  ((a 1)
    (b 2)
    (c 3)|};
      [%expect
        {|
        Unexpected end of input. Did you forget 1 closing parentheses?

        4|     (c 3)
                   ^
        |}]
    ;;

    let%expect_test "unmatched closed parenthesis" =
      Tester.print_test_t
        {|
  ((a 1)
   (b 2)
   (c 3)))|};
      [%expect
        {|
        Unmatched parenthesis

        4|    (c 3)))
                    ^
        |}]
    ;;

    let%expect_test "parenthesis inside quotes don't raise mismatch error" =
      Tester.print_test_t
        {|
  ((a "\(1")
   (b "2\)")
   (c "3\)"))|};
      [%expect {| |}]
    ;;

    let%expect_test "unmatched quote" =
      Tester.print_test_t
        {|
  ((a "a)
   (b 2)
   (c 3))|};
      [%expect
        {|
        [Parse Error] Unterminated string
        2|   ((a "a)
                 ^^^
        3|    (b 2)
           ^^^^^^^^
        4|    (c 3))
           ^^^^^^^^^
        Unexpected end of input. Did you forget 2 closing parentheses?

        2|   ((a "a)
               ^
        |}]
    ;;
  end)
;;
