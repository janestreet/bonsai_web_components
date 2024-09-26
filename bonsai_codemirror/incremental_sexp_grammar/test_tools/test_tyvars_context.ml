open! Core
module Context = Codemirror_incremental_sexp_grammar.Tyvars_context

let could_not_retrieve = Error.of_string "Could not retrieve tyvar value"

let retrieve_or_error c name =
  Context.resolve c name |> Or_error.of_option ~error:could_not_retrieve
;;

let print_res = function
  | Ok res -> print_s [%sexp (res : Sexp_grammar.grammar)]
  | Error err -> print_s [%sexp (err : Error.t)]
;;

let retrieve_and_print ctx name =
  let fst =
    let%bind.Or_error ctx in
    retrieve_or_error ctx name
  in
  print_res fst
;;

let%expect_test "Can set and retrieve one context" =
  let ctx = Context.register_many Context.empty [ "a" ] [ Integer ] in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}]
;;

let%expect_test "Can set and retrieve two" =
  let ctx = Context.register_many Context.empty [ "a"; "b" ] [ Integer; Float ] in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}];
  retrieve_and_print ctx "b";
  [%expect {| Float |}]
;;

let%expect_test "setting multiple times works and then removing works." =
  let ctx =
    let%bind.Or_error v1 = Context.register_many Context.empty [ "a" ] [ Integer ] in
    Context.register_many v1 [ "a" ] [ Float ]
  in
  retrieve_and_print ctx "a";
  [%expect {| Float |}];
  let ctx' =
    let%bind.Or_error ctx in
    Context.unregister_many ctx [ "a" ]
  in
  retrieve_and_print ctx' "a";
  [%expect {| Integer |}]
;;

let%expect_test "setting cyclical works" =
  let ctx =
    let%bind.Or_error v1 = Context.register_many Context.empty [ "a" ] [ Integer ] in
    let%bind.Or_error v2 = Context.register_many v1 [ "a" ] [ Tyvar "a" ] in
    let%bind.Or_error v3 = Context.register_many v2 [ "b" ] [ Tyvar "a" ] in
    let%bind.Or_error v4 = Context.register_many v3 [ "c" ] [ Tyvar "b" ] in
    Context.register_many v4 [ "a" ] [ Tyvar "c" ]
  in
  retrieve_and_print ctx "a";
  [%expect {| Integer |}];
  retrieve_and_print ctx "b";
  [%expect {| Integer |}];
  retrieve_and_print ctx "c";
  [%expect {| Integer |}]
;;

let%test_module "errors" =
  (module struct
    let%expect_test "trying to access non-existent tyvars doesn't work" =
      retrieve_and_print (Ok Context.empty) "a";
      [%expect {| "Could not retrieve tyvar value" |}];
      let ctx = Context.register_many Context.empty [ "a" ] [ Integer ] in
      retrieve_and_print ctx "b";
      [%expect {| "Could not retrieve tyvar value" |}];
      let ctx' =
        let%bind.Or_error ctx in
        Context.unregister_many ctx [ "a" ]
      in
      retrieve_and_print ctx' "a";
      [%expect {| "Could not retrieve tyvar value" |}]
    ;;

    let%expect_test "registering unequal lengths doesn't work" =
      let ctx = Context.register_many Context.empty [ "a" ] [ Integer; Float ] in
      retrieve_and_print ctx "a";
      [%expect
        {| "Internal error: unequal length lists for defn's list of tyvars and the tyvars defined in the tycon" |}];
      let ctx = Context.register_many Context.empty [ "a"; "b" ] [ Float ] in
      retrieve_and_print ctx "a";
      [%expect
        {| "Internal error: unequal length lists for defn's list of tyvars and the tyvars defined in the tycon" |}]
    ;;

    let%expect_test "unregistering non-existent tyvars doesn't work" =
      let ctx = Context.unregister_many Context.empty [ "a" ] in
      retrieve_and_print ctx "a";
      [%expect
        {| ("Internal error: couldn't unregister tyvar" (to_remove a) (tyvars ())) |}]
    ;;

    let%expect_test "registering cyclic that depends on non-existent tyvars doesn't work" =
      let ctx = Context.register_many Context.empty [ "a" ] [ Tyvar "a" ] in
      retrieve_and_print ctx "a";
      [%expect
        {|
        ("Internal error: self-referencing tyvar without a definition" (name a)
         (tyvars ()))
        |}];
      let ctx = Context.register_many Context.empty [ "a" ] [ Tyvar "b" ] in
      retrieve_and_print ctx "a";
      [%expect
        {|
        ("Internal error: self-referencing tyvar without a definition" (name a)
         (tyvars ()))
        |}]
    ;;
  end)
;;
