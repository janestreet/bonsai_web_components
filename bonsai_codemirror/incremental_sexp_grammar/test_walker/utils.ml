open Core
module Tokenizer = Codemirror_incremental_sexp_grammar.Tokenizer
module Walker = Codemirror_incremental_sexp_grammar.Sexp_grammar_walker
module Syntax_error = Codemirror_incremental_sexp_grammar.Syntax_error
module Lint_error = Codemirror_incremental_sexp_grammar.Lint_error
module Grammar_tools = Codemirror_incremental_sexp_grammar.Grammar_tools

let bprint_syntax_errors ~buf ~input_string errors =
  List.iter errors ~f:(fun err ->
    bprintf buf "%s\n" (Syntax_error.to_string_referencing_input ~input_string err))
;;

let bprint_lint_errors ~buf ~input_string errors =
  bprint_syntax_errors ~buf ~input_string (List.map errors ~f:Lint_error.to_syntax_error)
;;

let generate_tokens_from_string ~buf string =
  let tokens, tok_errors, tokenizer =
    string |> Tokenizer.process_string Tokenizer.initial
  in
  let end_tokens, grammar_errors = Tokenizer.process_eoi tokenizer in
  (match tok_errors @ grammar_errors with
   | [] -> ()
   | errors -> bprint_syntax_errors ~buf ~input_string:string errors);
  tokens @ end_tokens
;;

let walk_tokens ~buf ~grammar s =
  let tokens = generate_tokens_from_string ~buf s in
  let walker =
    List.fold tokens ~init:(Walker.create grammar) ~f:(fun t token ->
      Walker.For_testing.assert_nothing_too_crazy t;
      match Walker.process t token with
      | [], t' -> t'
      | errors, t' ->
        bprint_lint_errors ~buf ~input_string:s errors;
        t')
  in
  Walker.end_of_input_errors walker |> bprint_lint_errors ~buf ~input_string:s
;;

let walk_tokens_and_get_errors s ~grammar =
  let buf = Buffer.create 1024 in
  walk_tokens ~buf s ~grammar;
  Buffer.contents buf
;;

let walk_tokens_and_print_errors s ~grammar =
  print_endline (walk_tokens_and_get_errors s ~grammar)
;;

let assert_no_errors sexp_string ~grammar =
  match walk_tokens_and_get_errors ~grammar sexp_string with
  | "" -> ()
  | errors ->
    Error.raise_s
      [%message
        "Unexpected errors"
          (errors : string)
          (sexp_string : string)
          (grammar : Sexp_grammar.grammar)]
;;

let assert_errors sexp_string ~grammar =
  match walk_tokens_and_get_errors ~grammar sexp_string with
  | "" ->
    Error.raise_s
      [%message
        "Expected errors, but parsed"
          (sexp_string : string)
          (grammar : Sexp_grammar.grammar)]
  | _ -> ()
;;

module type S = sig
  type t [@@deriving sexp, sexp_grammar, quickcheck]
end

let quick_test_compliant_from_type
  ?trials
  ~sexp_of
  (grammar : 'a Sexp_grammar.t)
  generator
  =
  Quickcheck.test ?trials generator ~f:(fun x ->
    assert_no_errors (sexp_of x |> Sexp.to_string) ~grammar:grammar.untyped)
;;

let quick_test_compliant_from_gen ?trials grammar =
  let generator = Sexp_grammar_validation.Private.Obedient_generator.create grammar in
  Quickcheck.test ?trials generator ~f:(fun x ->
    assert_no_errors (Sexp.to_string x) ~grammar:grammar.untyped)
;;

let quick_test_noncompliant_from_gen ?trials grammar =
  let generator = Sexp_grammar_validation.Private.Disobedient_generator.create grammar in
  Quickcheck.test ?trials generator ~f:(fun (x, _) ->
    assert_errors (Sexp.to_string x) ~grammar:grammar.untyped)
;;

let quickcheck_tests ~sexp_of grammar generator =
  quick_test_compliant_from_type ~sexp_of grammar generator;
  quick_test_compliant_from_gen grammar;
  quick_test_noncompliant_from_gen grammar
;;

module Make_sexp_tester (M : S) = struct
  let print_test_t sexp_string =
    walk_tokens_and_print_errors sexp_string ~grammar:M.t_sexp_grammar.untyped
  ;;

  let quickcheck_tests () =
    quickcheck_tests ~sexp_of:M.sexp_of_t M.t_sexp_grammar M.quickcheck_generator
  ;;

  let print_grammar () = Grammar_tools.to_string M.t_sexp_grammar.untyped |> print_endline
end
