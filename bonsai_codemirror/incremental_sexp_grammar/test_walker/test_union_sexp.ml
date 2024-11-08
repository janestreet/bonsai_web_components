open! Core
open Sexp_grammar

module%test [@name "Union of atoms"] _ = struct
  let grammar = Union [ Bool; Char; Integer ]
  let print_test_union = Utils.walk_tokens_and_print_errors ~grammar
  let%expect_test "quickcheck" = Utils.quickcheck_tests_no_type { untyped = grammar }

  let%expect_test "valid union (bool)" =
    print_test_union "False";
    [%expect {| |}]
  ;;

  let%expect_test "valid union (char)" =
    print_test_union "a";
    [%expect {| |}]
  ;;

  let%expect_test "valid union (integer)" =
    print_test_union "42";
    [%expect {| |}]
  ;;

  let%expect_test "incorrect capitalization" =
    print_test_union "FALSE";
    [%expect
      {|
      Please fix one of the following errors:
      - Bool values must be true or false.
      - Expected a single character, like a or 1.
      - Expected an integer value, like -3 or 10.
      Parsed: `FALSE`.
      Incorrect capitalization; maybe you meant `false`?

      1| FALSE
         ^^^^^
      |}]
  ;;

  let%expect_test "invalid union (list)" =
    print_test_union "(a b c)";
    [%expect
      {|
      Please fix one of the following errors:
      - Expected a bool.
      - Expected a char.
      - Expected an int.
      Parsed: `(`.

      1| (a b c)
         ^
      |}]
  ;;

  let%expect_test "invalid union" =
    print_test_union "hello";
    [%expect
      {|
      Please fix one of the following errors:
      - Bool values must be true or false.
      - Expected a single character, like a or 1.
      - Expected an integer value, like -3 or 10.
      Parsed: `hello`.

      1| hello
         ^^^^^
      |}]
  ;;
end

module%test [@name "Union of lists"] _ = struct
  let grammar : Sexp_grammar.grammar =
    Union
      [ (list_sexp_grammar int_sexp_grammar).untyped
      ; (list_sexp_grammar char_sexp_grammar).untyped
      ]
  ;;

  let%expect_test "quickcheck" =
    Utils.quickcheck_tests_no_type { untyped = grammar };
    [%expect {| |}]
  ;;

  let test = Utils.walk_tokens_and_print_errors ~grammar

  let%expect_test "Fits both" =
    test "(1 2 3)";
    [%expect {| |}]
  ;;

  let%expect_test "fits one" =
    test "(1 2 3 54 0 2 4)";
    [%expect {| |}]
  ;;

  let%expect_test "fits neither" =
    test "(1 2 3 54 0 2 4 abc 2)";
    [%expect
      {|
      Expected an integer value, like -3 or 10.
      Parsed: `abc`.

      1| (1 2 3 54 0 2 4 abc 2)
                         ^^^
      |}];
    test "(1 2 3 abc 3 5 1 6)";
    [%expect
      {|
      Please fix one of the following errors:
      - Expected an integer value, like -3 or 10.
      - Expected a single character, like a or 1.
      Parsed: `abc`.

      1| (1 2 3 abc 3 5 1 6)
                ^^^
      |}]
  ;;
end

module%test [@name "Non-disjoint Union"] _ = struct
  let grammar : Sexp_grammar.grammar = Union [ Integer; Integer ]
  let test = Utils.walk_tokens_and_print_errors ~grammar
  let%expect_test "quickcheck" = Utils.quickcheck_tests_no_type { untyped = grammar }

  let%expect_test "works" =
    test "123";
    [%expect {| |}]
  ;;

  let%expect_test "errors aren't duplicated" =
    test "123a";
    [%expect
      {|
      Expected an integer value, like -3 or 10.
      Parsed: `123a`.

      1| 123a
         ^^^^
      |}]
  ;;
end

module%test [@name "Empty union"] _ = struct
  let grammar : Sexp_grammar.grammar = Union []
  let test = Utils.walk_tokens_and_print_errors ~grammar

  let%expect_test "quickcheck is impossible because we can't generate empty unions" =
    Expect_test_helpers_base.require_does_raise (fun () ->
      Utils.quickcheck_tests_no_type { untyped = grammar });
    [%expect {| "Base_quickcheck.Generator.of_list: empty list" |}]
  ;;

  let%expect_test "always errors" =
    test "(1 2 3)";
    [%expect
      {|
      Invalid sexp grammar: empty union
      Parsed: `(`.

      1| (1 2 3)
         ^
      |}];
    test "abc";
    [%expect
      {|
      Invalid sexp grammar: empty union
      Parsed: `abc`.

      1| abc
         ^^^
      |}]
  ;;
end

module%test [@name "Nested union"] _ = struct
  let leaf_union =
    Union
      [ List (Many String)
      ; List (Cons (Integer, Cons (Char, Cons (String, Empty))))
      ; List (Many Char)
      ]
  ;;

  let grammar =
    Union
      [ Union [ leaf_union; leaf_union ]
      ; Union [ Union [ leaf_union ]; Union [ leaf_union ] ]
      ; leaf_union
      ]
  ;;

  let%expect_test "quickcheck" =
    Utils.quickcheck_tests_no_type { untyped = grammar };
    [%expect {| |}]
  ;;

  let test = Utils.walk_tokens_and_print_errors ~grammar

  let%expect_test "works" =
    test "(1 2 3 4 )";
    [%expect {| |}]
  ;;

  let%expect_test "errors aren't duplicated" =
    test "123a";
    [%expect
      {|
      Please fix one of the following errors:
      - Expected a list of <string>
      - Expected a list, where the first element is a <int>
      - Expected a list of <char>
      Parsed: `123a`.
      Maybe you meant `(123a)`?

      1| 123a
         ^^^^
      |}]
  ;;
end

module Fruit = struct
  type t =
    | Apple
    | Blueberry
    | Banana
    | Pineapple
    | Custom of string
  [@@deriving sexp, equal, sexp_grammar, quickcheck]
end

module%test [@name "blang"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = Fruit.t Blang.t [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "quickcheck" =
    Tester.quickcheck_tests ();
    [%expect {| |}]
  ;;

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <recursive> |}]
  ;;

  let%expect_test "Regression: recovers after error in union" =
    Tester.print_test_t "(and Apple invalid)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| (and Apple invalid)
                    ^^^^^^^
      |}];
    Tester.print_test_t "(and (and Apple invalid Apple) Banana)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| (and (and Apple invalid Apple) Banana)
                         ^^^^^^^
      |}]
  ;;

  let%expect_test "regression: this one had duplicate error messages in a union, and an \
                   unmatched parenthesis warning."
    =
    Tester.print_test_t "(and Apple Bannana (an))";
    [%expect
      {|
      Unknown variant name.
      Parsed: `Bannana`.
      Possible spelling mistake; maybe you meant `Banana`?

      1| (and Apple Bannana (an))
                    ^^^^^^^
      Unknown variant name.
      Parsed: `an`.

      1| (and Apple Bannana (an))
                             ^^
      |}]
  ;;
end

module%test [@name "blang list"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = Fruit.t Blang.t list [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "quickcheck" =
    Tester.quickcheck_tests ();
    [%expect {| |}]
  ;;

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <list<recursive>> |}]
  ;;

  let%expect_test "Regression: recovers after error in union (list)" =
    Tester.print_test_t "((and Apple invalid))";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| ((and Apple invalid))
                     ^^^^^^^
      |}];
    Tester.print_test_t "((and Apple invalid) Apple (and Apple Banana))";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| ((and Apple invalid) Apple (and Apple Banana))
                     ^^^^^^^
      |}];
    Tester.print_test_t "((and Apple invalid Pineapple) Apple (and Apple Banana))";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| ((and Apple invalid Pineapple) Apple (and Apple Banana))
                     ^^^^^^^
      |}];
    Tester.print_test_t "((and Apple invalid Pineapple) Apple (and Apple Banana))";
    [%expect
      {|
      Unknown variant name.
      Parsed: `invalid`.

      1| ((and Apple invalid Pineapple) Apple (and Apple Banana))
                     ^^^^^^^
      |}];
    Tester.print_test_t "(pen Pineapple Apple pen)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `pen`.

      1| (pen Pineapple Apple pen)
          ^^^
      Unknown variant name.
      Parsed: `pen`.

      1| (pen Pineapple Apple pen)
                              ^^^
      |}]
  ;;
end

module%test [@name "regression: union > tycon > variant"] _ = struct
  let grammar =
    Union
      [ Tycon
          ( "t"
          , [ Integer ]
          , [ { tycon = "t"
              ; tyvars = [ "a" ]
              ; grammar =
                  Variant
                    { case_sensitivity = Case_sensitive_except_first_character
                    ; clauses =
                        [ No_tag
                            { name = "variant"
                            ; clause_kind = List_clause { args = Cons (Tyvar "a", Empty) }
                            }
                        ]
                    }
              }
            ] )
      ]
  ;;

  let test = Utils.walk_tokens_and_print_errors ~grammar
  let%expect_test "quickcheck" = Utils.quickcheck_tests_no_type { untyped = grammar }

  let%expect_test "works" =
    test "(Variant 10)";
    [%expect {| |}]
  ;;
end
