open Core

module Combined_tester1 = Utils.Make_sexp_tester (struct
    type t =
      | T0
      | T1 of bool * int * char list
      | T2 of string option
      | T3 of
          { foo : int
          ; bar : bool
          ; baz : char list
          }
      | T4 of [ `A | `B of [ `C | `D of int option ] ]
      | T5 of float list
      | T6 of t
    [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "invalid char list" =
  Combined_tester1.print_test_t
    {|
    (T3
     (foo 42)
     (bar false)
     (baz (a b cd e)))|};
  [%expect
    {|
    Expected a single character, like a or 1.
    Parsed: `cd`.

    5|      (baz (a b cd e)))
                      ^^
    |}]
;;

let%expect_test "incorrect capitilization of polymorphic variant" =
  Combined_tester1.print_test_t
    {|
    (T4
     (B
      (d (42))))|};
  [%expect
    {|
    Unknown variant name.
    Parsed: `d`.
    Incorrect capitalization; maybe you meant `D`?

    4|       (d (42))))
              ^
    |}]
;;

let%expect_test "float value for int" =
  Combined_tester1.print_test_t
    {|
    (T6
     (T6
      (T6
       (T6
        (T6
         (T1
           true
           42.0
           () ))))))|};
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `42.0`.
    Maybe you meant `42`?

    9|            42.0
                  ^^^^
    |}]
;;

module Combined_tester2 = Utils.Make_sexp_tester (struct
    type t =
      { t1 : bool * int * char list
      ; t2 : string option list
      ; t3 : [ `A | `B | `C ]
      }
    [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "invalid char list" =
  Combined_tester2.print_test_t
    {|
    ((t1
      (true 42 (a b cd e)))
     (t2
      ((hello)
       (world)
       ()))
     (t3 A))|};
  [%expect
    {|
    Expected a single character, like a or 1.
    Parsed: `cd`.

    3|       (true 42 (a b cd e)))
                           ^^
    |}]
;;

let%expect_test "incorrect capitalization of polymorphic variant" =
  Combined_tester2.print_test_t
    {|
    ((t1
      (true 42 (a b c d e)))
     (t2
      ((hello)
       (world)
       ()))
     (t3 d))|};
  [%expect
    {|
    Unknown variant name.
    Parsed: `d`.
    Possible spelling mistake; maybe you meant `A`, `B` or `C`?

    8|      (t3 d))
                ^
    |}]
;;

module Combined_tester3 = Utils.Make_sexp_tester (struct
    type t = (bool * int * char list) option list
    [@@deriving sexp, sexp_grammar, quickcheck]
  end)

let%expect_test "float for integer" =
  Combined_tester3.print_test_t
    {| (
          ((true 42 ()))
          (Some (true 42.0 (a b c d)))
          ()
          None)|};
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `42.0`.
    Maybe you meant `42`?

    3|           (Some (true 42.0 (a b c d)))
                             ^^^^
    |}]
;;
