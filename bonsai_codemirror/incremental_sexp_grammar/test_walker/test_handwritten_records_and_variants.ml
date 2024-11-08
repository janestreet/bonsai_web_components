open! Core
open Sexp_grammar

module%test [@name "Fields with wacky args"] _ = struct
  let grammar : Sexp_grammar.grammar =
    List
      (Fields
         { allow_extra_fields = false
         ; fields =
             [ No_tag { name = "empty"; required = true; args = Empty }
             ; No_tag { name = "empty_opt"; required = false; args = Empty }
             ; No_tag { name = "many_int"; required = true; args = Many Integer }
             ; No_tag { name = "many_int_opt"; required = false; args = Many Integer }
             ; No_tag
                 { name = "many"
                 ; required = true
                 ; args = Many (List (Cons (Char, Cons (Bool, Empty))))
                 }
             ; No_tag
                 { name = "many_opt"
                 ; required = false
                 ; args = Many (List (Cons (Char, Cons (Bool, Empty))))
                 }
             ; No_tag
                 { name = "cons"
                 ; required = true
                 ; args =
                     Cons
                       ( List (Cons (Bool, Empty))
                       , Cons (Bool, Cons (List (Many Float), Empty)) )
                 }
             ; No_tag
                 { name = "cons_opt"
                 ; required = false
                 ; args =
                     Cons
                       ( List (Cons (Bool, Empty))
                       , Cons (Bool, Cons (List (Many Float), Empty)) )
                 }
             ; No_tag
                 { name = "fields"
                 ; required = true
                 ; args =
                     Fields
                       { allow_extra_fields = false
                       ; fields =
                           [ No_tag { name = "a"; required = true; args = Empty }
                           ; No_tag
                               { name = "b"
                               ; required = true
                               ; args = Cons (Integer, Empty)
                               }
                           ]
                       }
                 }
             ; No_tag
                 { name = "fields_opt"
                 ; required = false
                 ; args =
                     Fields
                       { allow_extra_fields = false
                       ; fields =
                           [ No_tag { name = "a"; required = true; args = Empty }
                           ; No_tag
                               { name = "b"
                               ; required = true
                               ; args = Cons (Integer, Empty)
                               }
                           ]
                       }
                 }
             ]
         })
  ;;

  let%expect_test "quickcheck" =
    Utils.quickcheck_tests_no_type { untyped = grammar };
    [%expect {| |}]
  ;;

  let test = Utils.walk_tokens_and_print_errors ~grammar

  let%expect_test "Valid" =
    test
      {|(
    (empty)
    (many_int 1 2 3)
    (many (a true)(b false))
    (cons (true) true (0.1 0.2 0.3))
    (fields (a) (b 1))
    )|};
    [%expect {| |}]
  ;;

  let%expect_test "invalid: args wrapped in parens" =
    test
      {|(
  (empty)
  (many_int (1 2 3))
  (many ((a true)(b false)))
  (cons ((true) true (0.1 0.2 0.3)))
  (fields ((a) (b 1)) )
  )|};
    [%expect
      {|
      Expected an int.
      Parsed: `(1 2 3)`.

      3|   (many_int (1 2 3))
                     ^^^^^^^
      Expected a char.
      Parsed: `(a true)`.

      4|   (many ((a true)(b false)))
                  ^^^^^^^^
      Expected a bool.
      Parsed: `(b false)`.

      4|   (many ((a true)(b false)))
                          ^^^^^^^^^
      Expected a bool.
      Parsed: `(true)`.
      Maybe you meant `true`?

      5|   (cons ((true) true (0.1 0.2 0.3)))
                  ^^^^^^
      Unexpected argument; this sexp has already received all its arguments.
      Parsed: `true(0.1 0.2 0.3)`.

      5|   (cons ((true) true (0.1 0.2 0.3)))
                         ^^^^^^^^^^^^^^^^^^
      Expected a bool.
      Parsed: `)`.

      5|   (cons ((true) true (0.1 0.2 0.3)))
                                            ^
      Record fields must start with a label.
      Maybe you meant `a` or `b`?

      6|   (fields ((a) (b 1)) )
                    ^
      Missing required fields: a, b

      6|   (fields ((a) (b 1)) )
                               ^
      |}]
  ;;
end

module%test [@name "Variants with wacky clause args"] _ = struct
  let grammar : Sexp_grammar.grammar =
    Variant
      { case_sensitivity = Case_sensitive
      ; clauses =
          [ No_tag { name = "empty"; clause_kind = List_clause { args = Empty } }
          ; No_tag
              { name = "many_int"; clause_kind = List_clause { args = Many Integer } }
          ; No_tag
              { name = "many"
              ; clause_kind =
                  List_clause { args = Many (List (Cons (Char, Cons (Bool, Empty)))) }
              }
          ; No_tag
              { name = "cons"
              ; clause_kind =
                  List_clause
                    { args =
                        Cons
                          ( List (Cons (Bool, Empty))
                          , Cons (Bool, Cons (List (Many Float), Empty)) )
                    }
              }
          ; No_tag
              { name = "fields"
              ; clause_kind =
                  List_clause
                    { args =
                        Fields
                          { allow_extra_fields = false
                          ; fields =
                              [ No_tag { name = "a"; required = true; args = Empty }
                              ; No_tag
                                  { name = "b"
                                  ; required = true
                                  ; args = Cons (Integer, Empty)
                                  }
                              ]
                          }
                    }
              }
          ]
      }
  ;;

  let%expect_test "quickcheck" =
    Utils.quickcheck_tests_no_type { untyped = grammar };
    [%expect {| |}]
  ;;

  let test = Utils.walk_tokens_and_print_errors ~grammar

  let%expect_test "Valid" =
    test {|(empty)|};
    [%expect {| |}];
    test {|(many_int)|};
    [%expect {| |}];
    test {|(many_int 1 2 3)|};
    [%expect {| |}];
    test {|(many)|};
    [%expect {| |}];
    test {|(many (a true) (b false))|};
    [%expect {| |}];
    test {|(cons (true) true (0.1 0.2 0.3))|};
    [%expect {| |}];
    test {|(fields (a) (b 1))|};
    [%expect {| |}]
  ;;

  let%expect_test "invalid: no wrapping parens" =
    test {|empty|};
    [%expect
      {|
      This variant is not atomic, and requires a list of two elements: the field name and the value, e.g., (empty <empty_list>)
      Parsed: `empty`.

      1| empty
         ^^^^^
      |}]
  ;;

  let%expect_test "invalid: args wrapped in parens" =
    test {|(many_int ())|};
    [%expect
      {|
      Expected an int.
      Parsed: `()`.

      1| (many_int ())
                   ^^
      |}];
    test {|(many_int (1 2 3))|};
    [%expect
      {|
      Expected an int.
      Parsed: `(1 2 3)`.

      1| (many_int (1 2 3))
                   ^^^^^^^
      |}];
    test {|(many ())|};
    [%expect
      {|
      Expected a char.
      Parsed: `)`.

      1| (many ())
                ^
      |}];
    test {|(many ((a true) (b false)))|};
    [%expect
      {|
      Expected a char.
      Parsed: `(a true)`.

      1| (many ((a true) (b false)))
                ^^^^^^^^
      Expected a bool.
      Parsed: `(b false)`.

      1| (many ((a true) (b false)))
                         ^^^^^^^^^
      |}];
    test {|(cons ((true) true (0.1 0.2 0.3)))|};
    [%expect
      {|
      Expected a bool.
      Parsed: `(true)`.
      Maybe you meant `true`?

      1| (cons ((true) true (0.1 0.2 0.3)))
                ^^^^^^
      Unexpected argument; this sexp has already received all its arguments.
      Parsed: `true(0.1 0.2 0.3)`.

      1| (cons ((true) true (0.1 0.2 0.3)))
                       ^^^^^^^^^^^^^^^^^^
      Expected a bool.
      Parsed: `)`.

      1| (cons ((true) true (0.1 0.2 0.3)))
                                          ^
      |}];
    test {|(fields ((a) (b 1)))|};
    [%expect
      {|
      Record fields must start with a label.
      Maybe you meant `a` or `b`?

      1| (fields ((a) (b 1)))
                  ^
      Missing required fields: a, b

      1| (fields ((a) (b 1)))
                            ^
      |}]
  ;;
end
