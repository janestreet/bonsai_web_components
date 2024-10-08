open! Core

module%test [@name "Basic record"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        { foo : bool list
        ; bar : int
        ; baz : string
        ; qux : bool [@sexp.bool]
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| ((foo <list<bool>>)(bar <int>)(baz <string>)...) |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "valid record" =
    Tester.print_test_t "((foo (true false)) (bar 42) (baz \"hello, world!\") (qux))";
    [%expect {| |}]
  ;;

  let%expect_test "incorrect size list for record" =
    Tester.print_test_t
      {|((foo (true false))
       (bar 42)
       (baz hello world!))|};
    [%expect
      {|
      Expected end of record field. All values in a record must be a list of one or two elements, e.g., (is_enabled) or (quantity 5).
      Parsed: `world!`.

      3|        (baz hello world!))
                           ^^^^^^
      |}]
  ;;

  let%expect_test "incorrect size list for boolean field of sexp.bool" =
    Tester.print_test_t
      {|((foo (true false))
       (bar 42)
       (baz hello)
       (qux true))|};
    [%expect
      {|
      Expected end of record field. All values in a record must be a list of one or two elements, e.g., (is_enabled) or (quantity 5).
      Parsed: `true`.

      4|        (qux true))
                     ^^^^
      |}]
  ;;

  let%expect_test "misspelled record field" =
    Tester.print_test_t
      {|((foo (true false))
       (barz 42)
       (baz hello world!))|};
    [%expect
      {|
      Invalid field name, and additional fields are not allowed.
      Parsed: `barz`.
      Possible spelling mistake; maybe you meant `bar` or `baz`?

      2|        (barz 42)
                 ^^^^
      Expected end of record field. All values in a record must be a list of one or two elements, e.g., (is_enabled) or (quantity 5).
      Parsed: `world!`.

      3|        (baz hello world!))
                           ^^^^^^
      Missing required fields: bar

      3|        (baz hello world!))
                                  ^
      |}]
  ;;

  let%expect_test "malformed record" =
    Tester.print_test_t "((foo (true false)) 4 (baz \"hello, world!\"))";
    [%expect
      {|
      Record field expected
      Parsed: `4`.

      1| ((foo (true false)) 4 (baz "hello, world!"))
                             ^
      Missing required fields: bar

      1| ((foo (true false)) 4 (baz "hello, world!"))
                                                    ^
      |}]
  ;;

  let%expect_test "incorrect capitalization of record field" =
    Tester.print_test_t "((foo (true false)) (Bar 42) (baz \"hello, world!\"))";
    [%expect
      {|
      Invalid field name, and additional fields are not allowed.
      Parsed: `Bar`.
      Incorrect capitalization; maybe you meant `bar`?

      1| ((foo (true false)) (Bar 42) (baz "hello, world!"))
                              ^^^
      Missing required fields: bar

      1| ((foo (true false)) (Bar 42) (baz "hello, world!"))
                                                           ^
      |}]
  ;;

  let%expect_test "missing record fields" =
    Tester.print_test_t "((foo (true false)))";
    [%expect
      {|
      Missing required fields: bar, baz

      1| ((foo (true false)))
                            ^
      |}]
  ;;

  let%expect_test "extra record field" =
    Tester.print_test_t "((foo (true false)) (bar 42) (baz hello) (extra field))";
    [%expect
      {|
      Invalid field name, and additional fields are not allowed.
      Parsed: `extra`.

      1| ((foo (true false)) (bar 42) (baz hello) (extra field))
                                                   ^^^^^
      |}]
  ;;

  let%expect_test "duplicate record field" =
    Tester.print_test_t "((foo (true false)) (bar 42) (bar 42) (baz hello))";
    [%expect
      {|
      This field was already used earlier in this record.
      Parsed: `bar`.

      1| ((foo (true false)) (bar 42) (bar 42) (baz hello))
                                       ^^^
      |}]
  ;;

  let%expect_test "propagates errors" =
    Tester.print_test_t "((foo (true false)) (bar 42.0) (baz \"hello, world!\"))";
    [%expect
      {|
      Integer values cannot be floats.
      Parsed: `42.0`.
      Maybe you meant `42`?

      1| ((foo (true false)) (bar 42.0) (baz "hello, world!"))
                                  ^^^^
      |}]
  ;;
end

module%test [@name "Allow extra fields"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t = { foo : bool list } [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| ((foo <list<bool>>)) |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "Extra fields allowed" =
    Tester.print_test_t "((foo (true false)) (bar 42))";
    [%expect
      {|
      This record is complete, and does not allow extra fields.
      Parsed: `bar`.

      1| ((foo (true false)) (bar 42))
                              ^^^
      |}]
  ;;
end

module%test [@name "Non-required fields"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t =
        { foo : bool list [@sexp.default [ true ]]
        ; bar : int [@sexp.default 42]
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "grammar_to_string" =
    Tester.print_grammar ();
    [%expect {| ((foo? <list<bool>>)(bar? <int>)) |}]
  ;;

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()
  let%expect_test "No fields" = Tester.print_test_t "()"

  let%expect_test "One field" =
    Tester.print_test_t "( (bar 42))";
    [%expect {| |}];
    Tester.print_test_t "((foo (true false)))";
    [%expect {| |}]
  ;;

  let%expect_test "All fields" =
    Tester.print_test_t "((foo (true false)))";
    [%expect {| |}]
  ;;
end

module%test [@name "Nested record"] _ = struct
  module Tester = Utils.Make_sexp_tester (struct
      type t' =
        { a : int
        ; b : string
        }
      [@@deriving sexp, sexp_grammar, quickcheck]

      type t =
        { foo : t'
        ; bar : float
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "quickcheck" = Tester.quickcheck_tests ()

  let%expect_test "Empty list provided" =
    Tester.print_test_t {|()|};
    [%expect
      {|
      Missing required fields: foo, bar

      1| ()
          ^
      |}]
  ;;

  let%expect_test "Empty list provided for field" =
    Tester.print_test_t {|(())|};
    [%expect
      {|
      Record fields must start with a label.
      Maybe you meant `foo` or `bar`?

      1| (())
           ^
      Missing required fields: foo, bar

      1| (())
            ^
      |}]
  ;;

  let%expect_test "missing inner record" =
    Tester.print_test_t {|((foo ()) (bar 5.0))|};
    [%expect
      {|
      Missing required fields: a, b

      1| ((foo ()) (bar 5.0))
                ^
      |}]
  ;;

  let%expect_test "mispelled outer record field doesn't cause errors in inner record." =
    Tester.print_test_t {|((fooz ((a 5) (b hello))) (bar 5.0))|};
    [%expect
      {|
      Invalid field name, and additional fields are not allowed.
      Parsed: `fooz`.
      Possible spelling mistake; maybe you meant `foo`?

      1| ((fooz ((a 5) (b hello))) (bar 5.0))
           ^^^^
      Missing required fields: foo

      1| ((fooz ((a 5) (b hello))) (bar 5.0))
                                            ^
      |}]
  ;;
end
