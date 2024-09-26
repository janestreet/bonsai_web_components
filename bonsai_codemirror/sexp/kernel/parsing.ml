open! Core
open Codemirror_incremental_sexp_grammar

module Action = struct
  type 'doc t =
    | Parse_some_lines of 'doc
    | Edit_starting_at_nth_line of int
end

module type Doc = sig
  module Line_iterator : sig
    type in_progress

    type t =
      | Done
      | In_progress of (in_progress * string)

    val next : in_progress -> t
  end

  type t

  val iter_lines : t -> start_line:int -> Line_iterator.t
end

module Tokenize_state = struct
  type ('doc, 'error) t =
    { doc : (module Doc with type t = 'doc)
    ; tokenizer_state : Tokenizer.t
    ; errors : 'error list
    ; error_conv : Syntax_error.t -> 'error
    ; is_done : bool
    }

  let create ~error_conv doc =
    { doc; tokenizer_state = Tokenizer.initial; errors = []; error_conv; is_done = false }
  ;;

  let is_done { is_done; _ } = is_done

  let run (type a error) (t : (a, error) t) ~num_lines ~first_line_index ~(doc : a) =
    let module Doc = (val t.doc) in
    let rec loop ~iterator ~lines_parsed tokenizer errors_acc tokens_acc =
      match iterator with
      | Doc.Line_iterator.Done ->
        let final_tokens, eoi_tokenizer_errors = Tokenizer.process_eoi tokenizer in
        ( tokenizer
        , lines_parsed
        , eoi_tokenizer_errors :: errors_acc
        , final_tokens :: tokens_acc
        , `Done )
      | _ when lines_parsed = num_lines ->
        tokenizer, lines_parsed, errors_acc, tokens_acc, `Not_done
      | In_progress (iterator, value) ->
        let tokenizer', errors, tokens =
          String.fold
            (value ^ "\n")
            ~init:(tokenizer, errors_acc, tokens_acc)
            ~f:(fun (tokenizer, errors_acc, tokens_acc) char ->
              let tokens, new_syntax_errors, tokenizer' =
                Tokenizer.process tokenizer char
              in
              tokenizer', new_syntax_errors :: errors_acc, tokens :: tokens_acc)
        in
        loop
          ~iterator:(Doc.Line_iterator.next iterator)
          ~lines_parsed:(lines_parsed + 1)
          tokenizer'
          errors
          tokens
    in
    let state, lines_parsed, errors_rev, tokens_rev, is_done =
      loop
        ~iterator:(Doc.iter_lines doc ~start_line:first_line_index)
        ~lines_parsed:0
        t.tokenizer_state
        []
        []
    in
    let tokens = List.rev tokens_rev |> List.concat in
    let errors = List.concat errors_rev |> List.map ~f:t.error_conv in
    let is_done =
      match is_done with
      | `Done -> true
      | `Not_done -> false
    in
    let new_t = { t with tokenizer_state = state; errors = errors @ t.errors; is_done } in
    new_t, lines_parsed, tokens
  ;;
end

module Parse_state = struct
  type 'error t =
    { parser_state : Sexp_grammar_walker.t
    ; error_conv : Syntax_error.t -> 'error
    ; errors : 'error list
    }

  let create ~error_conv grammar =
    { parser_state = Sexp_grammar_walker.create grammar; error_conv; errors = [] }
  ;;

  let parse_tokens (t : _ t) tokenize_state tokens =
    let lint_errors, parser_state =
      List.fold tokens ~init:([], t.parser_state) ~f:(fun (errors, parser_state) token ->
        let errors', new_state = Sexp_grammar_walker.process parser_state token in
        errors' @ errors, new_state)
    in
    let eoi_lint_errors =
      if tokenize_state.Tokenize_state.is_done
      then Sexp_grammar_walker.end_of_input_errors parser_state
      else []
    in
    let errors =
      lint_errors @ eoi_lint_errors
      |> List.map ~f:(fun e -> Lint_error.to_syntax_error e |> t.error_conv)
    in
    { t with parser_state; errors = errors @ t.errors }
  ;;
end

module Checkpoint_state_generic = struct
  module Advance_checkpoint_result = struct
    type 'checkpoint t =
      { new_checkpoint : 'checkpoint
      ; lines_parsed : int
      ; is_done : bool
      ; tokens : Tokenizer.Token.t list
      }
  end

  type ('checkpoint, 'doc, 'error) t =
    { checkpoint_every_n_lines : int
    ; is_done : 'checkpoint -> bool
    ; errors : 'checkpoint -> 'error list
    ; empty_checkpoint : 'checkpoint
    ; checkpoints : 'checkpoint Checkpoints.t
    ; advance_checkpoint :
        'checkpoint
        -> 'doc
        -> num_lines:int
        -> first_line:int
        -> 'checkpoint Advance_checkpoint_result.t
    }

  let create
    ?(checkpoint_every_n_lines = 100)
    ~is_done
    ~errors
    ~empty_checkpoint
    ~advance_checkpoint
    ()
    =
    { checkpoint_every_n_lines
    ; is_done
    ; errors
    ; advance_checkpoint
    ; empty_checkpoint
    ; checkpoints = Checkpoints.empty
    }
  ;;

  let errors_unsorted t =
    match Checkpoints.get_last_including_end_of_parse t.checkpoints with
    | None -> []
    | Some checkpoint -> t.errors checkpoint
  ;;

  let is_done t =
    match Checkpoints.get_last_including_end_of_parse t.checkpoints with
    | None -> false
    | Some checkpoint -> t.is_done checkpoint
  ;;

  let get_starting_pos_and_checkpoint ?before t =
    match Checkpoints.get_last_checkpoint t.checkpoints ?before () with
    | None -> 0, t.empty_checkpoint
    | Some (parsed_up_to, checkpoint) -> parsed_up_to + 1, checkpoint
  ;;

  let parse_and_add_checkpoint t ~doc =
    let first_line_index, start_checkpoint = get_starting_pos_and_checkpoint t in
    let { Advance_checkpoint_result.new_checkpoint; lines_parsed; is_done; tokens = _ } =
      t.advance_checkpoint
        start_checkpoint
        doc
        ~num_lines:t.checkpoint_every_n_lines
        ~first_line:first_line_index
    in
    let last_line_parsed = first_line_index + lines_parsed - 1 in
    match
      Checkpoints.add_checkpoint
        t.checkpoints
        ~at_end_of_parse:is_done
        ~data:new_checkpoint
        ~pos:last_line_parsed
        ()
    with
    | Ok checkpoints -> { t with checkpoints }
    | Error err ->
      print_s [%message "SEXP PARSING IMPLEMENTATION ERROR" (err : Error.t)];
      t
  ;;

  let max_lines_to_redecorate = 300

  let adhoc_tokenization t ~starting_at_line ~until_line ~doc =
    let first_line_index, start_checkpoint =
      get_starting_pos_and_checkpoint ~before:starting_at_line t
    in
    let num_lines = until_line - first_line_index + 1 in
    match num_lines <= max_lines_to_redecorate with
    | false -> None
    | true ->
      let { Advance_checkpoint_result.tokens; _ } =
        t.advance_checkpoint start_checkpoint doc ~num_lines ~first_line:first_line_index
      in
      Some tokens
  ;;

  let apply_action t = function
    | Action.Edit_starting_at_nth_line n ->
      let valid_checkpoints = Checkpoints.discard_checkpoints t.checkpoints ~after:n in
      { t with checkpoints = valid_checkpoints }
    | Parse_some_lines doc -> if is_done t then t else parse_and_add_checkpoint t ~doc
  ;;
end

module Grammared = struct
  include Checkpoint_state_generic

  type ('doc, 'error) t =
    ( ('doc, 'error) Tokenize_state.t * 'error Parse_state.t
      , 'doc
      , 'error )
      Checkpoint_state_generic.t

  let create ?checkpoint_every_n_lines ~error_conv doc grammar =
    let empty_checkpoint =
      let tokenize = Tokenize_state.create ~error_conv doc in
      let parse = Parse_state.create ~error_conv grammar in
      tokenize, parse
    in
    let is_done (tokenize, _) = Tokenize_state.is_done tokenize in
    let errors (tokenize, parse) =
      tokenize.Tokenize_state.errors @ parse.Parse_state.errors
    in
    let advance_checkpoint (start_tokenize, start_parse) doc ~num_lines ~first_line =
      let tokenize_state, lines_parsed, tokens =
        Tokenize_state.run start_tokenize ~num_lines ~first_line_index:first_line ~doc
      in
      let parse_state = Parse_state.parse_tokens start_parse tokenize_state tokens in
      { Advance_checkpoint_result.new_checkpoint = tokenize_state, parse_state
      ; lines_parsed
      ; is_done = tokenize_state.is_done
      ; tokens
      }
    in
    Checkpoint_state_generic.create
      ?checkpoint_every_n_lines
      ~is_done
      ~errors
      ~empty_checkpoint
      ~advance_checkpoint
      ()
  ;;
end

module Ungrammared = struct
  include Checkpoint_state_generic

  type ('doc, 'error) t =
    (('doc, 'error) Tokenize_state.t, 'doc, 'error) Checkpoint_state_generic.t

  let create ?checkpoint_every_n_lines ~error_conv doc =
    let empty_checkpoint = Tokenize_state.create ~error_conv doc in
    let is_done tokenize = Tokenize_state.is_done tokenize in
    let errors tokenize = tokenize.Tokenize_state.errors in
    let advance_checkpoint start_tokenize doc ~num_lines ~first_line =
      let tokenize_state, lines_parsed, tokens =
        Tokenize_state.run start_tokenize ~num_lines ~first_line_index:first_line ~doc
      in
      { Advance_checkpoint_result.new_checkpoint = tokenize_state
      ; lines_parsed
      ; is_done = tokenize_state.is_done
      ; tokens
      }
    in
    Checkpoint_state_generic.create
      ?checkpoint_every_n_lines
      ~is_done
      ~errors
      ~empty_checkpoint
      ~advance_checkpoint
      ()
  ;;
end
