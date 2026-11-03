open! Core
open! Bonsai.Let_syntax
open! Import
open! Bonsai_web_test
module Private = Bonsai.Private
module Auto_generated = Bonsai_web_auto_generated

let count_computation_nodes name c =
  let skeleton =
    Private.Skeleton.Computation.of_computation (Private.top_level_handle c)
  in
  let o =
    object
      inherit [int] Private.Skeleton.Traverse.fold as super
      method! computation c acc = super#computation c (acc + 1)
    end
  in
  let count = o#computation skeleton 0 in
  print_endline (sprintf "%s: %d nodes" name count);
  count
;;

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

let test_form (type a) (module M : S with type t = a) (ts : a list) =
  let run ~optimize c =
    let handle = Handle.create ~optimize (form_result_spec M.sexp_of_t) c in
    List.iter ts ~f:(fun t ->
      Handle.do_actions handle [ t ];
      Handle.recompute_view handle)
  in
  let computation = Auto_generated.form (module M) ~allow_updates_when_focused:`Never in
  (* We reuse the `Computation.t` for a run through an unoptimized and an optimized
     version of the test in order to shake out any `Lazy` computations that would get
     forced. This allows us to report a more accurate count of the computations since we
     can look through the lazy. *)
  let raw_computation = Private.top_level_handle computation in
  run (fun graph -> Private.perform graph raw_computation) ~optimize:false;
  let preprocessed_computation = Private.pre_process raw_computation in
  run (fun graph -> Private.perform graph preprocessed_computation) ~optimize:true;
  ignore
    (count_computation_nodes "after preprocessing" (fun graph ->
       Private.perform graph preprocessed_computation)
     : int)
;;

let%expect_test "int" =
  test_form (module Int) [ 5 ];
  [%expect {| after preprocessing: 33 nodes |}]
;;

let%expect_test "option>variant>record form" =
  let module T = struct
    type record =
      { a : int
      ; b : string
      }
    [@@deriving sexp, sexp_grammar]

    type variant =
      | A
      | B of record
    [@@deriving sexp, sexp_grammar]

    type t = variant option [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ None; Some A; Some (B { a = 5; b = "hello" }) ];
  [%expect {| after preprocessing: 162 nodes |}]
;;

let%expect_test "option>variant form" =
  let module T = struct
    type variant =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]

    type t = variant option [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ None; Some A; Some (B 5) ];
  [%expect {| after preprocessing: 116 nodes |}]
;;

let%expect_test "variant form" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ A; B 5 ];
  [%expect {| after preprocessing: 79 nodes |}]
;;

let%expect_test "record form" =
  let module T = struct
    type t =
      { a : int
      ; b : string
      }
    [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ { a = 5; b = "hello" } ];
  [%expect {| after preprocessing: 79 nodes |}]
;;

let%expect_test "option form" =
  let module T = struct
    type t = int option [@@deriving sexp, sexp_grammar]
  end
  in
  test_form (module T) [ None; Some 5 ];
  [%expect {| after preprocessing: 70 nodes |}]
;;
