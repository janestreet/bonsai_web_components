open Core

let%test_module "lesser-known option representations" =
  (module struct
    module Tester = Utils.Make_sexp_tester (struct
        type t = string option [@@deriving sexp, sexp_grammar, quickcheck]
      end)

    let%expect_test "grammar_to_string" =
      Tester.print_grammar ();
      [%expect {| <option<string>> |}]
    ;;

    let%expect_test "quickcheck regression" =
      Tester.print_test_t "(somE\"\")";
      [%expect
        {|
        Options only accept one argument.
        Parsed: `""`.

        1| (somE"")
                ^^
        |}]
    ;;

    let%expect_test "quickcheck" = Tester.quickcheck_tests ()

    let%expect_test "all option formats are accepted" =
      List.iter
        [ "()"
        ; "(test)"
        ; "None"
        ; "(Some test)"
        ; "(Some)"
        ; "none"
        ; "(some test)"
        ; "(some)"
        ]
        ~f:Tester.print_test_t;
      [%expect {| |}]
    ;;
  end)
;;

let%test_module "basic option" =
  (module struct
    module Tester = Utils.Make_sexp_tester (struct
        type t = bool option [@@deriving sexp, sexp_grammar, quickcheck]
      end)

    let%expect_test "grammar_to_string" =
      Tester.print_grammar ();
      [%expect {| <option<bool>> |}]
    ;;

    let%expect_test "quickcheck" = Tester.quickcheck_tests ()

    let%expect_test "misspelled none" =
      Tester.print_test_t "Noen";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        Parsed: `Noen`.
        Possible spelling mistake; maybe you meant `None`?

        1| Noen
           ^^^^
        |}]
    ;;

    let%expect_test "improper capitalization - none" =
      Tester.print_test_t "NONE";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        Parsed: `NONE`.
        Incorrect capitalization; maybe you meant `None`?

        1| NONE
           ^^^^
        |}]
    ;;

    let%expect_test "straight up wrong" =
      Tester.print_test_t "asdfsdaf";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        Parsed: `asdfsdaf`.

        1| asdfsdaf
           ^^^^^^^^
        |}]
    ;;

    let%expect_test "misspelled some" =
      Tester.print_test_t "(Soem value)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `Soem`.
        Possible spelling mistake; maybe you meant `Some`?

        1| (Soem value)
            ^^^^
        |}]
    ;;

    let%expect_test "improper capitalization - some" =
      Tester.print_test_t "(SOME value)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `SOME`.
        Incorrect capitalization; maybe you meant `Some`?

        1| (SOME value)
            ^^^^
        |}]
    ;;

    let%expect_test "parenthesized none" =
      Tester.print_test_t "(None)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `None`.
        Possible spelling mistake; maybe you meant `Some`?

        1| (None)
            ^^^^
        |}]
    ;;

    let%expect_test "missing value" =
      Tester.print_test_t "(Some)";
      [%expect
        {|
        Bool values must be true or false.
        Parsed: `Some`.

        1| (Some)
            ^^^^
        |}]
    ;;

    let%expect_test "extra inputs: first is valid" =
      Tester.print_test_t "(Some true true)";
      [%expect
        {|
        Options only accept one argument.
        Parsed: `true`.

        1| (Some true true)
                      ^^^^
        |}]
    ;;

    let%expect_test "extra inputs: first is invalid" =
      Tester.print_test_t "(Some 5 5)";
      [%expect
        {|
        Bool values must be true or false.
        Parsed: `5`.

        1| (Some 5 5)
                 ^
        |}]
    ;;

    let%expect_test "Invalid start of option" =
      Tester.print_test_t "((Some) value)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Expected a bool.

        Parsed: `(Some)`.

        1| ((Some) value)
            ^^^^^^
        |}];
      Tester.print_test_t "((Some) 5 5)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Expected a bool.

        Parsed: `(Some)`.

        1| ((Some) 5 5)
            ^^^^^^
        |}];
      Tester.print_test_t "((X) 5 5)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Expected a bool.

        Parsed: `(X)`.

        1| ((X) 5 5)
            ^^^
        |}];
      Tester.print_test_t "(INCORRECT 5 5)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `INCORRECT`.

        1| (INCORRECT 5 5)
            ^^^^^^^^^
        |}]
    ;;

    let%expect_test "list for option" =
      Tester.print_test_t "(foo bar baz)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `foo`.

        1| (foo bar baz)
            ^^^
        |}]
    ;;

    let%expect_test "propagates errors" =
      Tester.print_test_t "(Some FALSE)";
      [%expect
        {|
        Bool values must be true or false.
        Parsed: `FALSE`.
        Incorrect capitalization; maybe you meant `false`?

        1| (Some FALSE)
                 ^^^^^
        |}]
    ;;

    let%expect_test "propagates errors - parentheses syntax" =
      Tester.print_test_t "(FALSE)";
      [%expect
        {|
        Expected None or (Some <bool>) for option type.
        If you're using the shorthand option format [(X) = Some X], the issue might be that:
        Bool values must be true or false.

        Parsed: `FALSE`.

        1| (FALSE)
            ^^^^^
        |}]
    ;;
  end)
;;

let%test_module "option of record" =
  (module struct
    module X = struct
      type t =
        { a : bool
        ; b : int
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end

    module Tester = Utils.Make_sexp_tester (struct
        type t = X.t option [@@deriving sexp, sexp_grammar, quickcheck]
      end)

    let%expect_test "grammar_to_string" =
      Tester.print_grammar ();
      [%expect {| <option((a ...)(b ...))> |}]
    ;;

    let%expect_test "quickcheck" = Tester.quickcheck_tests ()

    let%expect_test "wrong input type" =
      Tester.print_test_t "(Some 2)";
      [%expect
        {|
        Expected a record that looks like ((a <bool>)(b <int>))
        Parsed: `2`.

        1| (Some 2)
                 ^
        |}];
      Tester.print_test_t "(Some (1 2 3))";
      [%expect
        {|
        Record field expected
        Parsed: `1`.

        1| (Some (1 2 3))
                  ^
        Record field expected
        Parsed: `2`.

        1| (Some (1 2 3))
                    ^
        Record field expected
        Parsed: `3`.

        1| (Some (1 2 3))
                      ^
        Missing required fields: a, b

        1| (Some (1 2 3))
                       ^
        |}]
    ;;

    let%expect_test "extra inputs: first is invalid" =
      Tester.print_test_t "(Some 2 3 true ())";
      [%expect
        {|
        Expected a record that looks like ((a <bool>)(b <int>))
        Parsed: `2`.

        1| (Some 2 3 true ())
                 ^
        |}]
    ;;

    let%expect_test "extra inputs: first is valid" =
      Tester.print_test_t "(Some ((a true) (b 3)) 2 true ((a true) (b 3)))";
      [%expect
        {|
        Options only accept one argument.
        Parsed: `2 true((a true)(b 3))`.

        1| (Some ((a true) (b 3)) 2 true ((a true) (b 3)))
                                  ^^^^^^^^^^^^^^^^^^^^^^^
        |}]
    ;;

    let%expect_test "missing argument" =
      Tester.print_test_t "(Some)";
      [%expect
        {|
        Expected a record that looks like ((a <bool>)(b <int>))
        Parsed: `Some`.

        1| (Some)
            ^^^^
        |}]
    ;;
  end)
;;
