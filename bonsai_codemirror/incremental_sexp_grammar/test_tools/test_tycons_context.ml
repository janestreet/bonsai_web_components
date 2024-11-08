open! Core
module Context = Codemirror_incremental_sexp_grammar.Tycons_context

module Tycons = struct
  open Context.Tycon

  let empty_grammar = Sexp_grammar.Union []

  let make ~tyvars ~args =
    { name = "ignore"
    ; args
    ; defns = [ { tycon = "ignore"; grammar = empty_grammar; tyvars } ]
    }
  ;;
end

let register ctx tycon =
  let%map.Or_error _, ctx' = Context.register ctx tycon in
  ctx'
;;

let print_res = function
  | Ok res -> print_s [%sexp (res : Sexp_grammar.grammar)]
  | Error err -> print_s [%sexp (err : Error.t)]
;;

let retrieve_and_print ctx name =
  let fst =
    let%bind.Or_error ctx in
    Context.resolve ctx name
  in
  print_res fst
;;

let pop ctx =
  let%bind.Or_error ctx in
  Context.pop ctx
;;

let%expect_test "Can set and retrieve one context" =
  let ctx = register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ]) in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}]
;;

let%expect_test "Can set and retrieve two" =
  let ctx =
    register Context.empty (Tycons.make ~tyvars:[ "a"; "b" ] ~args:[ Integer; Float ])
  in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}];
  retrieve_and_print ctx "b";
  [%expect {| Float |}]
;;

let%expect_test "setting multiple times works and then removing works." =
  let ctx =
    let%bind.Or_error v1 =
      register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ])
    in
    register v1 (Tycons.make ~tyvars:[ "a" ] ~args:[ Float ])
  in
  retrieve_and_print ctx "a";
  [%expect {| Float |}];
  let ctx = pop ctx in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}]
;;

let%expect_test "setting cyclical works" =
  let ctx =
    let%bind.Or_error v1 =
      register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ])
    in
    let%bind.Or_error v2 =
      register v1 (Tycons.make ~tyvars:[ "a" ] ~args:[ Tyvar "a" ])
    in
    let%bind.Or_error v3 =
      register v2 (Tycons.make ~tyvars:[ "b" ] ~args:[ Tyvar "a" ])
    in
    let%bind.Or_error v4 =
      register v3 (Tycons.make ~tyvars:[ "c" ] ~args:[ Tyvar "b" ])
    in
    register v4 (Tycons.make ~tyvars:[ "a" ] ~args:[ Tyvar "c" ])
  in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}];
  let ctx = pop ctx in
  retrieve_and_print ctx "c";
  [%expect {| Integer |}];
  let ctx = pop ctx in
  retrieve_and_print ctx "b";
  [%expect {| Integer |}];
  let ctx = pop ctx in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}];
  let ctx = pop ctx in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}]
;;

module Nested_thing = struct
  [@@@disable_unused_warnings]

  type 'd recursive_variant =
    | Leaf of 'd
    | Chain of 'd recursive_variant
  [@@deriving sexp_grammar]

  type 'c record = { field : 'c } [@@deriving sexp_grammar]
  type 'b variant = Variant of 'b [@@deriving sexp_grammar]
  type 'a t = 'a variant record option recursive_variant list [@@deriving sexp_grammar]
end

let%expect_test "[Recursive] in tyvar points to the [Tycon] that provided the tyvar" =
  let ctx =
    let%bind.Or_error v1 =
      register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ])
    in
    register v1 (Tycons.make ~tyvars:[ "a" ] ~args:[ Recursive ("bar", [ Tyvar "a" ]) ])
  in
  retrieve_and_print ctx "a";
  [%expect
    {| (Tycon bar (Integer) (((tycon ignore) (tyvars (a)) (grammar (Union ()))))) |}]
;;

let%expect_test "Tyvar is defined as nested of other tyvar" =
  let ctx =
    let%bind.Or_error v1 =
      register
        Context.empty
        (Tycons.make ~tyvars:[ "a" ] ~args:[ Union [ Integer; Float ] ])
    in
    register
      v1
      (Tycons.make
         ~tyvars:[ "b" ]
         ~args:[ (Nested_thing.t_sexp_grammar { untyped = Tyvar "a" }).untyped ])
  in
  retrieve_and_print ctx "b";
  [%expect
    {|
    (List
     (Many
      (Tycon recursive_variant
       ((Option
         (List
          (Fields
           ((allow_extra_fields false)
            (fields
             ((No_tag
               ((name field) (required true)
                (args
                 (Cons
                  (Variant
                   ((case_sensitivity Case_sensitive_except_first_character)
                    (clauses
                     ((No_tag
                       ((name Variant)
                        (clause_kind
                         (List_clause
                          (args (Cons (Union (Integer Float)) Empty))))))))))
                  Empty)))))))))))
       (((tycon recursive_variant) (tyvars (d))
         (grammar
          (Variant
           ((case_sensitivity Case_sensitive_except_first_character)
            (clauses
             ((No_tag
               ((name Leaf)
                (clause_kind (List_clause (args (Cons (Tyvar d) Empty))))))
              (No_tag
               ((name Chain)
                (clause_kind
                 (List_clause
                  (args (Cons (Recursive recursive_variant ((Tyvar d))) Empty))))))))))))))))
    |}]
;;

module%test [@name "errors"] _ = struct
  let%expect_test "trying to access non-existent tyvars doesn't work" =
    retrieve_and_print (Ok Context.empty) "a";
    [%expect {| "No tyvars registered" |}];
    let ctx = register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ]) in
    retrieve_and_print ctx "b";
    [%expect {| ("unbound [Tyvar]" (name b)) |}];
    let ctx' =
      let%bind.Or_error ctx in
      Context.pop ctx
    in
    retrieve_and_print ctx' "a";
    [%expect {| "No tyvars registered" |}]
  ;;

  let%expect_test "trying to access non-existent tyvars through nested doesn't work" =
    let ctx =
      let%bind.Or_error v1 =
        register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer ])
      in
      register
        v1
        (Tycons.make
           ~tyvars:[ "b" ]
           ~args:[ (Nested_thing.t_sexp_grammar { untyped = Tyvar "c" }).untyped ])
    in
    retrieve_and_print ctx "b";
    [%expect {| ("unbound [Tyvar]" (name c)) |}]
  ;;

  let%expect_test "registering unequal lengths doesn't work" =
    let ctx =
      register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Integer; Float ])
    in
    retrieve_and_print ctx "a";
    [%expect
      {|
      ("wrong number of type variable parameters" (defn.tyvars (a))
       (tycon.args (Integer Float)))
      |}];
    let ctx = register Context.empty (Tycons.make ~tyvars:[ "a"; "b" ] ~args:[ Float ]) in
    retrieve_and_print ctx "a";
    [%expect
      {|
      ("wrong number of type variable parameters" (defn.tyvars (a b))
       (tycon.args (Float)))
      |}]
  ;;

  let%expect_test "poping non-existent tyvars doesn't work" =
    let ctx = Context.pop Context.empty in
    retrieve_and_print ctx "a";
    [%expect {| "No tyvars registered" |}]
  ;;

  let%expect_test "registering cyclic that depends on non-existent tyvars doesn't work" =
    let ctx = register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Tyvar "a" ]) in
    retrieve_and_print ctx "a";
    [%expect {| "cannot use [Tyvar] unless within the scope of a [Tycon]" |}];
    let ctx = register Context.empty (Tycons.make ~tyvars:[ "a" ] ~args:[ Tyvar "b" ]) in
    retrieve_and_print ctx "a";
    [%expect {| "cannot use [Tyvar] unless within the scope of a [Tycon]" |}];
    let ctx =
      register
        Context.empty
        (Tycons.make
           ~tyvars:[ "a" ]
           ~args:[ (Nested_thing.t_sexp_grammar { untyped = Tyvar "a" }).untyped ])
    in
    retrieve_and_print ctx "a";
    [%expect {| "cannot use [Tyvar] unless within the scope of a [Tycon]" |}]
  ;;
end
