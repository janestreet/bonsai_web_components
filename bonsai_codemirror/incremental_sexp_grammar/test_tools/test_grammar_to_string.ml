open! Core

[@@@disable_unused_warnings]

let test grammar =
  Codemirror_incremental_sexp_grammar.Grammar_tools.to_string grammar |> print_endline
;;

module type Grammarable = sig
  type t [@@deriving sexp_grammar]
end

let test_for_type (module G : Grammarable) = test G.t_sexp_grammar.untyped

module type Grammarable' = sig
  type 'a t [@@deriving sexp_grammar]
end

let test_for_type' (module G_prime : Grammarable') (module G : Grammarable) =
  test (G_prime.t_sexp_grammar G.t_sexp_grammar).untyped
;;

let%expect_test "atoms" =
  test (Sexp_grammar.Any "");
  [%expect {| <any> |}];
  test Sexp_grammar.Bool;
  [%expect {| <bool> |}];
  test Sexp_grammar.Char;
  [%expect {| <char> |}];
  test Sexp_grammar.Float;
  [%expect {| <float> |}];
  test Sexp_grammar.Integer;
  [%expect {| <int> |}];
  test Sexp_grammar.String;
  [%expect {| <string> |}]
;;

let%expect_test "lists of atoms" =
  test_for_type
    (module struct
      type t = bool list [@@deriving sexp_grammar]
    end);
  [%expect {| <list<bool>> |}];
  test_for_type
    (module struct
      type t = int list [@@deriving sexp_grammar]
    end);
  [%expect {| <list<int>> |}];
  test_for_type
    (module struct
      type t = string list [@@deriving sexp_grammar]
    end);
  [%expect {| <list<string>> |}];
  test_for_type
    (module struct
      type t = string list list list [@@deriving sexp_grammar]
    end);
  [%expect {| <list<list<list...>>> |}]
;;

let%expect_test "variants" =
  test_for_type
    (module struct
      type t =
        [ `A
        | `B
        ]
      [@@deriving sexp_grammar]
    end);
  [%expect {| <variant(A)|(B)> |}];
  test_for_type
    (module struct
      type t =
        [ `Foo of int
        | `Bar of string
        ]
      [@@deriving sexp_grammar]
    end);
  [%expect {| <variant(Foo <int>)|(Bar <string>)> |}]
;;

let%expect_test "options" =
  test_for_type
    (module struct
      type t = bool option [@@deriving sexp_grammar]
    end);
  [%expect {| <option<bool>> |}];
  test_for_type
    (module struct
      type t = int option [@@deriving sexp_grammar]
    end);
  [%expect {| <option<int>> |}];
  test_for_type
    (module struct
      type t = string option [@@deriving sexp_grammar]
    end);
  [%expect {| <option<string>> |}]
;;

let%expect_test "records" =
  test_for_type
    (module struct
      type t = { x : int } [@@deriving sexp_grammar]
    end);
  [%expect {| ((x <int>)) |}];
  test_for_type
    (module struct
      type t =
        { x : int
        ; y : string option
        }
      [@@deriving sexp_grammar]
    end);
  [%expect {| ((x <int>)(y <option<string>>)) |}]
;;

let%expect_test "recursive types" =
  test_for_type
    (module struct
      type a = Variant of t
      and t = a list [@@deriving sexp_grammar]
    end);
  [%expect {| <recursive> |}];
  test_for_type
    (module struct
      type t =
        | Leaf
        | Node of t
      [@@deriving sexp_grammar]
    end);
  [%expect {| <recursive> |}]
;;

let%expect_test "polymorphic types" =
  test_for_type'
    (module struct
      type 'a t = 'a option [@@deriving sexp_grammar]
    end)
    (module Int);
  [%expect {| <option<int>> |}];
  test_for_type'
    (module struct
      type 'a t = 'a list [@@deriving sexp_grammar]
    end)
    (module String);
  [%expect {| <list<string>> |}]
;;
