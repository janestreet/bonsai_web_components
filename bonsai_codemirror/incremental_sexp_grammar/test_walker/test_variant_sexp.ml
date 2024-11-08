open! Core

module%test [@name "basic variant"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        | Foo of bool list
        | Bar of int
        | Baz
        | Balloon of { x : int }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar to sexp" =
    Tester.print_grammar_sexp ();
    [%expect
      {|
      (Lazy
       (Variant
        ((case_sensitivity Case_sensitive_except_first_character)
         (clauses
          ((No_tag
            ((name Foo)
             (clause_kind (List_clause (args (Cons (List (Many Bool)) Empty))))))
           (No_tag
            ((name Bar) (clause_kind (List_clause (args (Cons Integer Empty))))))
           (No_tag ((name Baz) (clause_kind Atom_clause)))
           (No_tag
            ((name Balloon)
             (clause_kind
              (List_clause
               (args
                (Fields
                 ((allow_extra_fields false)
                  (fields
                   ((No_tag ((name x) (required true) (args (Cons Integer Empty))))))))))))))))))
      |}]
  ;;

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <variant(Foo <list<bool>>)|(Bar <int>)|(Baz)...> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid variant" =
    Tester.print_test_t "(Foo (true false True False))";
    [%expect {| |}]
  ;;

  let%expect_test "valid variant" =
    Tester.print_test_t "Baz";
    [%expect {| |}]
  ;;

  let%expect_test "propagates errors" =
    Tester.print_test_t "(Bar 42.0)";
    [%expect
      {|
      Integer values cannot be floats.
      Parsed: `42.0`.
      Maybe you meant `42`?

      1| (Bar 42.0)
              ^^^^
      |}]
  ;;

  let%expect_test "variant of record" =
    Tester.print_test_t "(Balloon (x 10) )";
    [%expect {| |}];
    Tester.print_test_t "(Balloon (x 10) (y 10))";
    [%expect
      {|
      This record is complete, and does not allow extra fields.
      Parsed: `y`.

      1| (Balloon (x 10) (y 10))
                          ^
      |}];
    Tester.print_test_t "(Balloon (x 10) (x 15))";
    [%expect
      {|
      This field was already used earlier in this record.
      Parsed: `x`.

      1| (Balloon (x 10) (x 15))
                          ^
      |}];
    Tester.print_test_t "(Balloon (x abc))";
    [%expect
      {|
      Expected an integer value, like -3 or 10.
      Parsed: `abc`.

      1| (Balloon (x abc))
                     ^^^
      |}];
    Tester.print_test_t "(Balloon (x 10) foo)";
    [%expect
      {|
      Record field expected
      Parsed: `foo`.

      1| (Balloon (x 10) foo)
                         ^^^
      |}]
  ;;

  let%expect_test "multiple arguments: first valid" =
    Tester.print_test_t "(Bar 42 42 50)";
    [%expect
      {|
      Unexpected argument: this variant does not expect any more arguments.
      Parsed: `42 50`.

      1| (Bar 42 42 50)
                 ^^^^^
      |}];
    Tester.print_test_t "(Foo (true) (true))";
    [%expect
      {|
      Unexpected argument: this variant does not expect any more arguments.
      Parsed: `(true)`.

      1| (Foo (true) (true))
                     ^^^^^^
      |}];
    Tester.print_test_t "(Foo (true) true)";
    [%expect
      {|
      Unexpected argument: this variant does not expect any more arguments.
      Parsed: `true`.

      1| (Foo (true) true)
                     ^^^^
      |}]
  ;;

  let%expect_test "multiple arguments: first invalid" =
    Tester.print_test_t "(Bar 42.0 42)";
    [%expect
      {|
      Integer values cannot be floats.
      Parsed: `42.0`.
      Maybe you meant `42`?

      1| (Bar 42.0 42)
              ^^^^
      |}];
    Tester.print_test_t "(Bar (42) 42)";
    [%expect
      {|
      Expected an int.
      Parsed: `(42)`.
      Maybe you meant `42`?

      1| (Bar (42) 42)
              ^^^^
      Unexpected argument: this variant does not expect any more arguments.
      Parsed: `42`.

      1| (Bar (42) 42)
                   ^^
      |}]
  ;;

  let%expect_test "unknown variant" =
    Tester.print_test_t "4";
    [%expect
      {|
      Unknown variant name.
      Parsed: `4`.

      1| 4
         ^
      |}]
  ;;

  let%expect_test "spelling mistake" =
    Tester.print_test_t "(Barz hello)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `Barz`.
      Possible spelling mistake; maybe you meant `Bar` or `Baz`?

      1| (Barz hello)
          ^^^^
      |}]
  ;;

  let%expect_test "valid - uncapitalized first character" =
    Tester.print_test_t "(bar 42)";
    [%expect {| |}]
  ;;

  let%expect_test "invalid - incorrect capitalization" =
    Tester.print_test_t "(BAR 42)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `BAR`.
      Incorrect capitalization; maybe you meant `Bar`?

      1| (BAR 42)
          ^^^
      |}]
  ;;

  let%expect_test "atomic variant with value" =
    Tester.print_test_t "(Baz value)";
    [%expect
      {|
      This variant is atomic and cannot have parentheses around it, or take an argument.
      Parsed: `Baz`.

      1| (Baz value)
          ^^^
      |}]
  ;;

  let%expect_test "non-atomic variant without parens or value" =
    Tester.print_test_t "Bar";
    [%expect
      {|
      This variant is not atomic, and requires a list of two elements: the field name and the value, e.g., (Bar <int>)
      Parsed: `Bar`.

      1| Bar
         ^^^
      |}]
  ;;

  let%expect_test "non-atomic variant with parens, without value" =
    Tester.print_test_t "(Bar)";
    [%expect
      {|
      This variant requires a payload of type <int>.

      1| (Bar)
             ^
      |}]
  ;;

  let%expect_test "no variant" =
    Tester.print_test_t ")";
    [%expect
      {|
      Expected a variant, got a closing paren.
      Parsed: `)`.

      1| )
         ^
      |}]
  ;;

  let%expect_test "no constructor" =
    Tester.print_test_t "(())";
    [%expect
      {|
      Expected variant constructor, got an opening paren.
      Parsed: `(`.

      1| (())
          ^
      |}];
    Tester.print_test_t "())";
    [%expect
      {|
      Expected variant constructor, got a closing paren.
      Parsed: `)`.

      1| ())
          ^
      Unmatched parenthesis

      1| ())
           ^
      |}]
  ;;

  let%expect_test "atom instead of list" =
    Tester.print_test_t "(Foo false)";
    [%expect
      {|
      Expected a list of <bool>
      Parsed: `false`.
      Maybe you meant `(false)`?

      1| (Foo false)
              ^^^^^
      |}]
  ;;

  let%expect_test "list instead of atom" =
    Tester.print_test_t "(Bar (42))";
    [%expect
      {|
      Expected an int.
      Parsed: `(42)`.
      Maybe you meant `42`?

      1| (Bar (42))
              ^^^^
      |}]
  ;;
end

module%test [@name "polymorphic variant"] _ = struct
  (* Polymorphic variant tests *)
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        [ `Foo of bool list
        | `Bar of int
        | `Baz
        ]
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <variant(Foo <list<bool>>)|(Bar <int>)|(Baz)> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  (* Regular variants are compared module the capitalization of the first character,
   but polymorphic variants are compared exactly.
  *)
  let%expect_test "invalid - uncapitalized first character" =
    Tester.print_test_t "(bar 42)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `bar`.
      Incorrect capitalization; maybe you meant `Bar`?

      1| (bar 42)
          ^^^
      |}]
  ;;

  let%expect_test "invalid - incorrect capitalization" =
    Tester.print_test_t "(BAR 42)";
    [%expect
      {|
      Unknown variant name.
      Parsed: `BAR`.
      Incorrect capitalization; maybe you meant `Bar`?

      1| (BAR 42)
          ^^^
      |}]
  ;;

  let%expect_test "invalid - incorrect capitalization" =
    Tester.print_test_t "(Foo true true false)";
    [%expect
      {|
      Expected a list of <bool>
      Parsed: `true`.
      Maybe you meant `(true)`?

      1| (Foo true true false)
              ^^^^
      |}]
  ;;

  let%expect_test "variant of empty list" =
    Tester.print_test_t "(Foo ())";
    [%expect {| |}];
    Tester.print_test_t "(Foo)";
    [%expect
      {|
      This variant requires a payload of type <list<bool>>.

      1| (Foo)
             ^
      |}];
    Tester.print_test_t "Foo";
    [%expect
      {|
      This variant is not atomic, and requires a list of two elements: the field name and the value, e.g., (Foo <list<bool>>)
      Parsed: `Foo`.

      1| Foo
         ^^^
      |}]
  ;;
end

module%test [@name "recursive variant"] _ = struct
  module Recursive_variant = Utils.Make_sexp_tester (struct
      type t =
        | Leaf of bool
        | Node of t list
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "quickcheck" = Recursive_variant.quickcheck_tests ()

  let%expect_test "valid recursive variant" =
    Recursive_variant.print_test_t
      {|(Node (
       (Leaf false)
       (Leaf true)
       (Node (
        (Leaf false)
        (Leaf true)
        (Leaf false)))))|};
    [%expect {| |}]
  ;;

  let%expect_test "invalid recursive variant" =
    Recursive_variant.print_test_t
      {|(Node (
       (Leaf false)
       (Leaf true)
       (Node (
        (Leaf false)
        (Leaf TRUE)
        (Leaf false)))))|};
    [%expect
      {|
      Bool values must be true or false.
      Parsed: `TRUE`.
      Incorrect capitalization; maybe you meant `true`?

      6|         (Leaf TRUE)
                       ^^^^
      |}]
  ;;
end

module%test [@name "Edge case: variant that looks like option"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        | None
        | Some of string
        | Extra
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <variant(None)|(Some <string>)|(Extra)> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "None" =
    Tester.print_test_t "None";
    [%expect {| |}]
  ;;

  let%expect_test "Some" =
    Tester.print_test_t "Some";
    [%expect
      {|
      This variant is not atomic, and requires a list of two elements: the field name and the value, e.g., (Some <string>)
      Parsed: `Some`.

      1| Some
         ^^^^
      |}];
    Tester.print_test_t "(Some)";
    [%expect
      {|
      This variant requires a payload of type <string>.

      1| (Some)
              ^
      |}];
    Tester.print_test_t "(Some hello)";
    [%expect {| |}];
    Tester.print_test_t "(Some some)";
    [%expect {| |}]
  ;;

  let%expect_test "Extra" =
    Tester.print_test_t "Extra";
    [%expect {| |}];
    Tester.print_test_t "(Extra)";
    [%expect
      {|
      This variant is atomic and cannot have parentheses around it, or take an argument.
      Parsed: `Extra`.

      1| (Extra)
          ^^^^^
      |}]
  ;;
end

module%test [@name "variant with customizations"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        | None
        | Some of string
        | Extra
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <variant(None)|(Some <string>)|(Extra)> |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "None" =
    Tester.print_test_t "None";
    [%expect {| |}]
  ;;
end

module%test [@name "variant with sexp flags"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        | A
        | B of int * float * t
        | C of int list
        | C' of int list [@sexp.list]
        | D of { foo : bool [@sexp.bool] }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_sexp" =
    Tester.print_grammar_sexp ();
    [%expect
      {|
      (Lazy
       (Tycon t ()
        (((tycon t) (tyvars ())
          (grammar
           (Variant
            ((case_sensitivity Case_sensitive_except_first_character)
             (clauses
              ((No_tag ((name A) (clause_kind Atom_clause)))
               (No_tag
                ((name B)
                 (clause_kind
                  (List_clause
                   (args (Cons Integer (Cons Float (Cons (Recursive t ()) Empty))))))))
               (No_tag
                ((name C)
                 (clause_kind
                  (List_clause (args (Cons (List (Many Integer)) Empty))))))
               (No_tag
                ((name C') (clause_kind (List_clause (args (Many Integer))))))
               (No_tag
                ((name D)
                 (clause_kind
                  (List_clause
                   (args
                    (Fields
                     ((allow_extra_fields false)
                      (fields
                       ((No_tag ((name foo) (required false) (args Empty)))))))))))))))))))))
      |}]
  ;;

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| <recursive> |}]
  ;;

  let%expect_test "Sanity check that unintuitive values parse" =
    List.iter [ "(D)" ] ~f:(fun value ->
      Expect_test_helpers_base.require_does_not_raise (fun () ->
        Tester.t_of_sexp (Sexp.of_string value) |> (ignore : Tester.t -> unit)))
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid" =
    Tester.print_test_t "A";
    [%expect {| |}];
    Tester.print_test_t "(B 1 2.0 A)";
    [%expect {| |}];
    Tester.print_test_t "(C (1 2 3 4 5))";
    [%expect {| |}];
    Tester.print_test_t "(C ())";
    [%expect {| |}];
    Tester.print_test_t "(C' 1 2 3 4 5)";
    [%expect {| |}];
    Tester.print_test_t "(C')";
    [%expect {| |}];
    Tester.print_test_t "(D)";
    [%expect {| |}]
  ;;
end
