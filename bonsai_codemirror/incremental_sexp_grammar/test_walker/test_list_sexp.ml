open! Core

module%test [@name "basic list"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = bool list [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <list<bool>> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid list" =
    Tester.print_test_t "(true false True False)";
    [%expect {| |}]
  ;;

  let%expect_test "valid list (empty)" =
    Tester.print_test_t "()";
    [%expect {| |}]
  ;;

  let%expect_test "atom instead of list" =
    Tester.print_test_t "Hello";
    [%expect
      {|
      Expected a list of <bool>
      Parsed: `Hello`.
      Maybe you meant `(Hello)`?

      1| Hello
         ^^^^^
      |}]
  ;;

  let%expect_test "invalid boolean" =
    Tester.print_test_t "(false 42)";
    [%expect
      {|
      Bool values must be true or false.
      Parsed: `42`.

      1| (false 42)
                ^^
      |}]
  ;;

  let%expect_test "parenthesized boolean" =
    Tester.print_test_t "((false))";
    [%expect
      {|
      Expected a bool.
      Parsed: `(false)`.
      Maybe you meant `false`?

      1| ((false))
          ^^^^^^^
      |}]
  ;;

  let%expect_test "nested list" =
    Tester.print_test_t "((false true false))";
    [%expect
      {|
      Expected a bool.
      Parsed: `(false true false)`.

      1| ((false true false))
          ^^^^^^^^^^^^^^^^^^
      |}]
  ;;

  let%expect_test "multiple nested" =
    Tester.print_test_t "((false) (true))";
    [%expect
      {|
      Expected a bool.
      Parsed: `(false)`.
      Maybe you meant `false`?

      1| ((false) (true))
          ^^^^^^^
      Expected a bool.
      Parsed: `(true)`.
      Maybe you meant `true`?

      1| ((false) (true))
                  ^^^^^^
      |}];
    Tester.print_test_t "((false) true)";
    [%expect
      {|
      Expected a bool.
      Parsed: `(false)`.
      Maybe you meant `false`?

      1| ((false) true)
          ^^^^^^^
      |}]
  ;;

  let%expect_test "propagates errors (multiline)" =
    Tester.print_test_t
      {|
          (true
           false
           TRUE
           FALSE)|};
    [%expect
      {|
      Bool values must be true or false.
      Parsed: `TRUE`.
      Incorrect capitalization; maybe you meant `true`?

      4|            TRUE
                    ^^^^
      Bool values must be true or false.
      Parsed: `FALSE`.
      Incorrect capitalization; maybe you meant `false`?

      5|            FALSE)
                    ^^^^^
      |}]
  ;;
end

module%test [@name "nested lists"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = bool list list [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <list<list<bool>>> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid list" =
    Tester.print_test_t "((true false) (True False) ())";
    [%expect {| |}]
  ;;

  let%expect_test "valid list (empty)" =
    Tester.print_test_t "()";
    [%expect {| |}]
  ;;

  let%expect_test "propagates errors" =
    Tester.print_test_t
      {|
    ((true false)
     (TRUE FALSE))|};
    [%expect
      {|
      Bool values must be true or false.
      Parsed: `TRUE`.
      Incorrect capitalization; maybe you meant `true`?

      3|      (TRUE FALSE))
               ^^^^
      Bool values must be true or false.
      Parsed: `FALSE`.
      Incorrect capitalization; maybe you meant `false`?

      3|      (TRUE FALSE))
                    ^^^^^
      |}]
  ;;
end

module%test [@name "record vs list"] _ = struct
  module X = struct
    type t = { a : int } [@@deriving sexp, equal, sexp_grammar]
  end

  module Y = struct
    type t = int list [@@deriving sexp, equal, sexp_grammar]
  end

  let%expect_test "Regression: error message is specialized to records or lists" =
    Utils.walk_tokens_and_print_errors ~grammar:X.t_sexp_grammar.untyped "a int";
    [%expect
      {|
      Expected a record that looks like ((a <int>))
      Parsed: `a`.

      1| a int
         ^
      |}];
    Utils.walk_tokens_and_print_errors ~grammar:Y.t_sexp_grammar.untyped "a int";
    [%expect
      {|
      Expected a list of <int>
      Parsed: `a`.
      Maybe you meant `(a)`?

      1| a int
         ^
      |}]
  ;;
end
