open Core

module%test [@name "deep recursive"] _ = struct
  type 'a t =
    | Leaf
    | Chain of 'a t
  [@@deriving sexp, sexp_grammar, quickcheck]

  module Quickcheck (S : Utils.S) = struct
    let%expect_test "quickcheck" =
      Utils.quickcheck_tests
        ~sexp_of:(sexp_of_t S.sexp_of_t)
        (t_sexp_grammar S.t_sexp_grammar)
        (quickcheck_generator S.quickcheck_generator)
    ;;
  end

  module%test [@name "int"] _ = Quickcheck (Int)
  module%test [@name "string"] _ = Quickcheck (String)

  module%test [@name "record"] _ = Quickcheck (struct
      type t =
        { a : int
        ; b : string
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  module%test [@name "variant"] _ = Quickcheck (struct
      type t =
        | Foo
        | Bar of int
        | Baz of string Blang.t
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  module%test [@name "self as parameter"] _ = Quickcheck (struct
      type 'a t' = 'a t [@@deriving sexp, sexp_grammar, quickcheck]
      type t = int t' [@@deriving sexp, sexp_grammar, quickcheck]
    end)
end

module%test [@name "mutually recursive"] _ = struct
  type ('a, 'b) t2 =
    { first : 'a t
    ; second : 'b t
    }

  and 'a t =
    | A of ('a, string) t2 option list [@sexp.list]
    | B
    | C of 'a * ('a, 'a) t2
    | D of [ `One | `Two ]
  [@@deriving sexp, sexp_grammar, quickcheck]

  module Quickcheck (S : Utils.S) = struct
    let%expect_test "quickcheck" =
      Utils.quickcheck_tests
        ~sexp_of:(sexp_of_t S.sexp_of_t)
        (t_sexp_grammar S.t_sexp_grammar)
        (quickcheck_generator S.quickcheck_generator)
    ;;
  end

  module%test [@name "int"] _ = Quickcheck (Int)
  module%test [@name "string"] _ = Quickcheck (String)

  module%test [@name "record"] _ = Quickcheck (struct
      type t =
        { a : int
        ; b : string
        }
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  module%test [@name "variant"] _ = Quickcheck (struct
      type t =
        | Foo
        | Bar of int
        | Baz of string Blang.t
      [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  module%test [@name "self as parameter"] _ = Quickcheck (struct
      type 'a t' = 'a t [@@deriving sexp, sexp_grammar, quickcheck]
      type t = int t' [@@deriving sexp, sexp_grammar, quickcheck]
    end)

  let%expect_test "nested error" =
    let test =
      Utils.walk_tokens_and_print_errors
        ~grammar:(t_sexp_grammar int_sexp_grammar).untyped
    in
    test "A";
    [%expect
      {|
      This variant is not atomic, and requires a list of two elements: the field name and the value, e.g., (A <list<option<recursive>>>)
      Parsed: `A`.

      1| A
         ^
      |}];
    test
      {|(C 5
         ((first abc) (second 4))
        )|};
    [%expect
      {|
      Unknown variant name.
      Parsed: `abc`.
      Possible spelling mistake; maybe you meant `A`, `B` or `C`?

      2|          ((first abc) (second 4))
                          ^^^
      Unknown variant name.
      Parsed: `4`.
      Possible spelling mistake; maybe you meant `A`, `B`, `C` or `D`?

      2|          ((first abc) (second 4))
                                       ^
      |}]
  ;;
end
