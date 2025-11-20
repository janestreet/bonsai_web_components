open! Core
open Js_of_ocaml

(* This test exists to enforce that [bonsai_web_ui_codemirror_read_only_bindings_only]
   does not pull in the codemirror js bundle. If the expect test output ever changes to
   [undefined], some dependency of [bonsai_web_ui_codemirror_read_only_bindings_only] has
   started depending on [codemirror] instead of [codemirror_bindings]. *)

let%expect_test "" =
  (* We don't want this to be dead code eliminated. *)
  Fn.ignore (Bonsai_web_ui_codemirror_read_only_bindings_only.make |> Obj.magic);
  let is_mock_for_testing_field =
    Js.Unsafe.js_expr {| String(codemirror.isMockForTesting) |} |> Js.to_string
  in
  print_endline is_mock_for_testing_field;
  (* Must not be [undefined]! *)
  [%expect {| isMockForTesting |}]
;;
