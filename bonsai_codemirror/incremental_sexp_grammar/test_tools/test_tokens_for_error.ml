open! Core
open Codemirror_incremental_sexp_grammar
open Tokenizer

let get_all_tokens string =
  let tokens, errors, t = process_string initial string in
  let end_tokens, end_errors = process_eoi t in
  tokens @ end_tokens, errors @ end_errors
;;

let test str =
  let tokens, _ = get_all_tokens str in
  match tokens with
  | [] -> ()
  | hd :: tl ->
    let open Tokens_for_error in
    let tokens = create hd tl in
    print_s
      [%message
        (offset_start tokens : int) (offset_end tokens : int) (to_string tokens : string)]
;;

let%expect_test "empty string" =
  test "";
  [%expect {| |}]
;;

let%expect_test "empty list" =
  test "()";
  [%expect
    {| (("offset_start tokens" 0) ("offset_end tokens" 2) ("to_string tokens" "()")) |}]
;;

let%expect_test "single token" =
  test "a";
  [%expect
    {| (("offset_start tokens" 0) ("offset_end tokens" 1) ("to_string tokens" a)) |}]
;;

let%expect_test "multiple tokens" =
  test "a b";
  [%expect
    {|
    (("offset_start tokens" 0) ("offset_end tokens" 3)
     ("to_string tokens" "a b"))
    |}]
;;

let%expect_test "exactly 12 tokens" =
  test "(((((())))))";
  [%expect
    {|
    (("offset_start tokens" 0) ("offset_end tokens" 12)
     ("to_string tokens" "(((((())))))"))
    |}]
;;

let%expect_test "a short sexp" =
  test "(Lorem (ipsum dolor sit amet) consectuter (adipiscing (elit (sed(do(eiusmod)))))";
  [%expect
    {|
    (("offset_start tokens" 0) ("offset_end tokens" 80)
     ("to_string tokens"
      "(Lorem(ipsum dolor sit amet)consectuter(adipiscing<truncated>)"))
    |}]
;;

let%expect_test "a long sexp" =
  test
    {|(library
  (name bonsai_web_ui_view)
  (public_name bonsai.web_ui_view)
  (libraries bonsai bonsai_web_ui_form_view bonsai_web_ui_toggle core
    virtual_dom.input_widgets virtual_dom)
  (preprocess
   (pps ppx_jane ppx_bonsai ppx_css))
  (flags :standard -alert -private_bonsai_view_library))

 (rule
  (targets card_style.ml card_style.mli card_style__generated.ml
    card_style__generated.mli)
  (deps card_style.css)
  (action
   (bash "%{bin:css_inliner} %{deps} \"((rewrite ()))\"")))|};
  [%expect
    {|
    (("offset_start tokens" 0) ("offset_end tokens" 491)
     ("to_string tokens"
      "(library(name bonsai_web_ui_view)(public_name bonsai.web_ui_view)(<truncated>)"))
    |}]
;;
