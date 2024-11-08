open! Core

module%test [@name "just the grammar"] _ = struct
  let test =
    Utils.walk_tokens_and_print_errors
      ~grammar:(List (Cons (Integer, Cons (Char, Many Bool))))
  ;;

  let%expect_test "valid cons" =
    test "(42 a true false True False)";
    [%expect {| |}]
  ;;

  let%expect_test "propagates errors" =
    test "(42 a true false TRUE False)";
    [%expect
      {|
      Bool values must be true or false.
      Parsed: `TRUE`.
      Incorrect capitalization; maybe you meant `true`?

      1| (42 a true false TRUE False)
                          ^^^^
      |}]
  ;;

  let%expect_test "empty list" =
    test "()";
    [%expect
      {|
      Expected an int.
      Parsed: `)`.

      1| ()
          ^
      |}]
  ;;
end

module%test [@name "tuple ending in list"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = int * bool * char list [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| [<int>::[<bool>::[...]]] |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid tuple" =
    Tester.print_test_t "(3 false (a b c))";
    [%expect {| |}]
  ;;

  let%expect_test "invalid tuple" =
    Tester.print_test_t "(3 false a b c)";
    [%expect
      {|
      Expected a list of <char>
      Parsed: `a`.
      Maybe you meant `(a)`?

      1| (3 false a b c)
                  ^
      |}]
  ;;

  let%expect_test "tuple w/ extra args" =
    Tester.print_test_t "(3 false (a b c) 3)";
    [%expect
      {|
      Unexpected argument; this sexp has already received all its arguments.
      Parsed: `3`.

      1| (3 false (a b c) 3)
                          ^
      |}]
  ;;

  let%expect_test "unnecessary parentheses" =
    Tester.print_test_t "((3 false a b c))";
    [%expect
      {|
      Expected an int.
      Parsed: `(3 false a b c)`.

      1| ((3 false a b c))
          ^^^^^^^^^^^^^^^
      |}]
  ;;
end

module%test [@name "tuple not ending in list"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = int * bool list * char [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| [<int>::[<list...>::[...]]] |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid tuple" =
    Tester.print_test_t "(3 (false) a)";
    [%expect {| |}]
  ;;

  let%expect_test "invalid tuple" =
    Tester.print_test_t "(3 false a b c)";
    [%expect
      {|
      Expected a list of <bool>
      Parsed: `false`.
      Maybe you meant `(false)`?

      1| (3 false a b c)
            ^^^^^
      |}]
  ;;

  let%expect_test "tuple w/ extra args" =
    Tester.print_test_t "(3 (false) a 5 (abc))";
    [%expect
      {|
      Unexpected argument; this sexp has already received all its arguments.
      Parsed: `5(abc)`.

      1| (3 (false) a 5 (abc))
                      ^^^^^^^
      |}]
  ;;

  let%expect_test "unnecessary parentheses" =
    Tester.print_test_t "((3 false a b c))";
    [%expect
      {|
      Expected an int.
      Parsed: `(3 false a b c)`.

      1| ((3 false a b c))
          ^^^^^^^^^^^^^^^
      |}]
  ;;
end
