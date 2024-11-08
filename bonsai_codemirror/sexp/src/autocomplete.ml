open! Core

module type Model = sig
  type t [@@deriving sexp_of]
end

open Codemirror
open Bonsai.Let_syntax
open Parsexp_prefix
module Protocol = Sexp_grammar_completion_protocol

module Completion = struct
  type t =
    { from : int
    ; to_ : int option
    ; options : string list
    ; exhaustive : bool
    }
  [@@deriving sexp_of]
end

let completions ~text ~cursor_position ~completer =
  let sexp_prefix = Sexp_prefix.of_substring ~pos:0 ~len:cursor_position text in
  match sexp_prefix with
  | None | Some (_ :: _, _) ->
    { Completion.from = cursor_position; options = []; to_ = None; exhaustive = true }
  (* The pattern below is a complement of the first pattern in this
     match-expression. In words, it matches the prefix of only the
     first sexp on a string. *)
  | Some (([], _) as sexp_prefix) ->
    let prefix, atom_prefix = Protocol.Prefix.of_sexp_prefix sexp_prefix in
    let exhaustive, options =
      match completer prefix with
      | Ok candidates ->
        let options =
          Protocol.Candidates.candidates candidates
          |> List.filter_map ~f:(fun (candidate : Protocol.Candidate.t) ->
            if Protocol.Candidate.matches_atom_prefix candidate atom_prefix
            then (
              match candidate with
              | Add_atom { atom_signified; _ } ->
                let signifier = Sexp.to_string (sexp_of_string atom_signified) in
                Some signifier
              | Enter_list -> None
              | Enter_list_and_add_atom { atom_signified; _ } ->
                let signifier = Sexp.to_string (sexp_of_string atom_signified) in
                Some ("(" ^ signifier))
            else None)
        in
        candidates.exhaustive, options
      | Error _ -> false, []
    in
    let from, to_ =
      match atom_prefix with
      | Some atom_prefix ->
        let signifier = Atom_prefix.get_signifier ~parser_input:text atom_prefix in
        let starts_with_quote = String.is_prefix signifier ~prefix:"\"" in
        let ends_with_quote =
          match String.get text cursor_position with
          | exception _ -> false
          | '"' -> true
          | _ -> false
        in
        let to_ =
          if starts_with_quote && ends_with_quote
          then cursor_position + 1
          else cursor_position
        in
        cursor_position - String.length signifier, to_
      | None -> cursor_position, cursor_position
    in
    { Completion.from; to_ = Some to_; options; exhaustive }
;;

let extension ?(include_non_exhaustive_hint = true) grammar =
  let completer = unstage (Sexp_grammar_completion.complete grammar) in
  let completion_source =
    Autocomplete.CompletionSource.of_sync_fun (fun context ->
      let text =
        context
        |> Autocomplete.CompletionContext.state
        |> State.Editor_state.doc
        |> Text.Text.to_json
        |> String.concat ~sep:"\n"
      in
      let cursor_position = Autocomplete.CompletionContext.pos context in
      let { Completion.from; to_; options; exhaustive } =
        completions ~text ~cursor_position ~completer
      in
      let options =
        List.map options ~f:(fun option ->
          Autocomplete.Completion.create ~label:option ())
      in
      let options =
        if (not exhaustive) && include_non_exhaustive_hint
        then (
          let last_option =
            Autocomplete.Completion.create
              ~label:"\"\""
              ~detail:"list is not exhaustive"
              ()
          in
          options @ [ last_option ])
        else options
      in
      Autocomplete.CompletionResult.create ~from ?to_ ~options ~filter:false ())
  in
  Autocomplete.autocompletion
    (Autocomplete.Config.create
       ~activate_on_typing:true
       ~override:[ completion_source ]
       ())
;;

module Grammar = struct
  type t = Sexp_grammar.grammar [@@deriving compare, equal, sexp_of]
end

let with_extension
  ?(extra_extension = Basic_setup.basic_setup)
  ?include_non_exhaustive_hint
  ~name
  grammar
  =
  Bonsai_web_ui_codemirror.with_dynamic_extensions
    (module Grammar)
    ~equal:[%equal: Grammar.t]
    ~name
    ~initial_state:
      (State.Editor_state.create
         (State.Editor_state_config.create ~extensions:[ extra_extension ] ()))
    ~compute_extensions:
      (Bonsai.return (fun grammar ->
         [ extension ?include_non_exhaustive_hint { untyped = grammar }; extra_extension ]))
    (let%map grammar in
     grammar.Sexp_grammar.untyped)
;;

module Private = struct
  module For_tests = struct
    module Completion = Completion

    let completions ~text ~cursor_position ~grammar =
      let completer = unstage (Sexp_grammar_completion.complete grammar) in
      completions ~text ~cursor_position ~completer
    ;;
  end
end
