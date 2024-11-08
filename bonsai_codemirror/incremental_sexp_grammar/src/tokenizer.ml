open! Core
open Parsexp_symbolic_automaton

module Token_type = struct
  type t =
    | Lparen
    | Rparen
    | String of string
  [@@deriving sexp_of, equal, compare]
end

module Token = struct
  type t =
    { token_type : Token_type.t
    ; nesting_level : int
    ; pos : Position.t
    }
  [@@deriving sexp_of, equal, compare]

  let to_sexp_string tokens =
    let buf = Buffer.create 16 in
    let _ : bool =
      List.fold tokens ~init:false ~f:(fun after_unquoted_atom token ->
        match token.token_type with
        | Token_type.Lparen ->
          Buffer.add_char buf '(';
          false
        | Rparen ->
          Buffer.add_char buf ')';
          false
        | String s ->
          let printed_atom = Sexp.to_string (Atom s) in
          let quoted = Char.(String.get printed_atom 0 = '"') in
          if after_unquoted_atom && not quoted then Buffer.add_char buf ' ' else ();
          Buffer.add_string buf printed_atom;
          not quoted)
    in
    Buffer.contents buf
  ;;
end

(* This is mostly copied from ../rainbow_parentheses/kernel/symbolic_automaton_based.ml
   and keeps track of the bare minimum needed to traverse the sexp. *)

module Base_state = struct
  type t =
    | Base of
        { state : Automaton.State.t
        ; sexp_comment_nesting : int list
        }
    | Block_comment of
        { state : Automaton.State.t
        ; rest : t
        }
  [@@deriving sexp_of]

  let initial_state = Base { state = Automaton.State.initial; sexp_comment_nesting = [] }
  let error_state = Base { state = Error; sexp_comment_nesting = [] }
  let current (Base { state; _ } | Block_comment { state; _ }) = state

  let set_current t state =
    match t with
    | Base base_state -> Base { base_state with state }
    | Block_comment comment_state -> Block_comment { comment_state with state }
  ;;

  let handle_end_of_sexp_additional_actions t ~nesting:current_nesting =
    match t with
    | Base { sexp_comment_nesting = []; _ } | Block_comment _ -> t
    | Base ({ sexp_comment_nesting = nesting :: rest; _ } as base) ->
      if current_nesting = nesting
      then Base { base with sexp_comment_nesting = rest }
      else t
  ;;

  let start_sexp_comment t ~nesting =
    match t with
    | Base ({ sexp_comment_nesting; _ } as base) ->
      Base { base with sexp_comment_nesting = nesting :: sexp_comment_nesting }
    | Block_comment _ -> error_state
  ;;

  let handle_ending_block_comment = function
    | Block_comment { rest = Block_comment _ as rest; _ } -> rest
    | Block_comment { rest; _ } -> set_current rest Whitespace
    | Base _ -> error_state
  ;;

  let in_sexp_comment t =
    match t with
    | Base { sexp_comment_nesting = _ :: _; _ } -> true
    | Block_comment _ | Base { sexp_comment_nesting = []; _ } -> false
  ;;
end

(* This builds on Base_state to accumulate atoms and eventually emit them as tokens. *)
module Token_emitting_state = struct
  type t =
    { base_state : Base_state.t
    ; nesting_level : int
    ; offset : int
    ; token_start : int option (* if within a token, records starting offset *)
    ; accumulated_errors : Syntax_error.t list
    ; atom_buffer : char list
    ; escaped_value : int
    }
  [@@deriving sexp_of]

  let initial_state =
    { base_state = Base_state.initial_state
    ; nesting_level = 0
    ; offset = 0
    ; token_start = None
    ; accumulated_errors = []
    ; atom_buffer = []
    ; escaped_value = 0
    }
  ;;

  let current { base_state; _ } = Base_state.current base_state
  let set_base t new_base = { t with base_state = new_base }
  let set_current t new_state = set_base t (Base_state.set_current t.base_state new_state)

  let change_nesting_by ({ nesting_level; _ } as t) delta =
    { t with nesting_level = nesting_level + delta }
  ;;

  let add_error t error =
    { t with
      accumulated_errors =
        { Syntax_error.error = "[Parse Error] " ^ error
        ; position =
            { Position.offset_start = Option.value t.token_start ~default:t.offset
            ; offset_end = t.offset
            }
        }
        :: t.accumulated_errors
    }
  ;;

  let add_char t char =
    { t with
      atom_buffer = char :: t.atom_buffer
    ; token_start = Option.first_some t.token_start (Some (t.offset - 1))
    }
  ;;

  let add_escape t base_multiplier value_or_error =
    match value_or_error with
    | Ok value -> { t with escaped_value = (t.escaped_value * base_multiplier) + value }
    | Error err ->
      { (add_error t (Error.to_string_hum err)) with
        escaped_value = t.escaped_value * base_multiplier
      }
  ;;

  let finish_escape t =
    let pushed =
      match Char.of_int t.escaped_value with
      | Some c -> add_char t c
      | None ->
        add_error t [%string "Escape code not in ASCII range: %{t.escaped_value#Int}"]
    in
    { pushed with escaped_value = 0 }
  ;;

  let push_atom t =
    let atom = List.rev t.atom_buffer |> String.of_char_list in
    let new_base =
      Base_state.handle_end_of_sexp_additional_actions
        t.base_state
        ~nesting:t.nesting_level
    in
    let new_state = set_base t new_base in
    atom, { new_state with atom_buffer = []; token_start = None; accumulated_errors = [] }
  ;;

  let create_token t ?(nesting_level_delta = 0) ?(offset_end_delta = 0) token_type
    : Token.t
    =
    { token_type
    ; nesting_level = t.nesting_level + nesting_level_delta
    ; pos =
        { offset_start = Option.value t.token_start ~default:(t.offset - 1)
        ; offset_end = t.offset + offset_end_delta
        }
    }
  ;;

  let if_not_in_sexp_comment t tokens =
    match Base_state.in_sexp_comment t.base_state with
    | true -> []
    | false -> tokens
  ;;

  let get_digit_val c f name =
    match f c with
    | None ->
      Or_error.error_string
        [%string "Should never call this function with a non-%{name} digit"]
    | Some i -> Or_error.return i
  ;;

  let dec_val c = get_digit_val c Char.get_digit "decimal"
  let hex_val c = get_digit_val c Char.get_hex_digit "hexadecimal"

  let handle_error (reason : Parse_error_reason.t) parser_state char =
    match reason with
    | Unexpected_char_parsing_dec_escape | Unexpected_char_parsing_hex_escape ->
      let err_state =
        add_error
          parser_state
          [%string "Unexpected character in escape sequence: %{char#Char}"]
      in
      let flushed_state = { err_state with escaped_value = 0 } in
      [], set_current flushed_state (Quoted_string Normal)
    | Unexpected_character_after_cr ->
      let err_state =
        add_error
          parser_state
          [%string "Unexpected character after carriage return: %{char#Char}"]
      in
      [], set_current err_state Whitespace
    | Comment_token_in_unquoted_atom ->
      let err_state =
        add_error parser_state "Comment tokens not allowed in unquoted atoms"
      in
      let new_state = set_current err_state (Unquoted_string Normal) in
      [], add_char new_state char
    | Unterminated_block_comment ->
      let err_state = add_error parser_state "Unterminated block comment" in
      [], err_state
    | Unterminated_quoted_string ->
      let err_state = add_error parser_state "Unterminated string" in
      [], err_state
    | Automaton_in_error_state ->
      (* This should not be reachable since we always manually transition out of
              error states. *)
      let err_state = add_error parser_state "Unknown tokenizer error :(" in
      [], set_base err_state Base_state.error_state
  ;;

  let process parser_state char =
    let parser_state = { parser_state with offset = parser_state.offset + 1 } in
    let rec handle_state parser_state =
      match Automaton.transition (current parser_state, char) with
      | T (Opening, current) ->
        let token =
          if_not_in_sexp_comment
            parser_state
            [ Token_type.Lparen |> create_token parser_state ]
        in
        let new_state = set_current parser_state current in
        token, change_nesting_by new_state 1
      | T (Closing, current) ->
        let token =
          if_not_in_sexp_comment
            parser_state
            [ Token_type.Rparen |> create_token parser_state ~nesting_level_delta:(-1) ]
        in
        let new_state = set_current parser_state current in
        (match parser_state.nesting_level with
         | 0 -> token, new_state
         | _ ->
           let decreased_nesting = change_nesting_by new_state (-1) in
           let new_base =
             Base_state.handle_end_of_sexp_additional_actions
               decreased_nesting.base_state
               ~nesting:decreased_nesting.nesting_level
           in
           token, set_base decreased_nesting new_base)
      | T (Start_block_comment, state) ->
        ( []
        , set_base parser_state (Block_comment { state; rest = parser_state.base_state })
        )
      | T (Start_sexp_comment, current) ->
        let new_state = set_current parser_state current in
        let new_base =
          Base_state.start_sexp_comment
            new_state.base_state
            ~nesting:new_state.nesting_level
        in
        [], set_base new_state new_base
      | T ((Add_atom_char | Add_quoted_atom_char | Add_first_char), current) ->
        let new_state = set_current parser_state current in
        [], add_char new_state char
      | T (Add_escaped, current) ->
        let new_state = set_current parser_state current in
        let state_with_char =
          match char with
          | 'n' -> add_char new_state '\n'
          | 'r' -> add_char new_state '\r'
          | 'b' -> add_char new_state '\b'
          | 't' -> add_char new_state '\t'
          | '\\' | '\'' | '"' -> add_char new_state char
          | _ -> add_char (add_char new_state '\\') char
        in
        [], state_with_char
      | T (Add_dec_escape_char, current) ->
        let new_state = set_current parser_state current in
        [], add_escape new_state 10 (dec_val char)
      | T (Add_hex_escape_char, current) ->
        let new_state = set_current parser_state current in
        [], add_escape new_state 16 (hex_val char)
      | T (Add_last_dec_escape_char, current) ->
        let new_state = set_current parser_state current in
        [], add_escape new_state 10 (dec_val char) |> finish_escape
      | T (Add_last_hex_escape_char, current) ->
        let new_state = set_current parser_state current in
        [], add_escape new_state 16 (hex_val char) |> finish_escape
      | T (Push_quoted_atom, current) ->
        let new_state = set_current parser_state current in
        let atom, new_state = push_atom new_state in
        ( if_not_in_sexp_comment
            parser_state
            [ Token_type.String atom |> create_token parser_state ]
        , new_state )
      | T (Start_quoted_string, current) ->
        let new_state = set_current parser_state current in
        [], { new_state with token_start = Some (parser_state.offset - 1) }
      | T
          ( (Nop | Add_token_char | Comment_add_last_dec_escape_char | Start_line_comment)
          , current ) -> [], set_current parser_state current
      | E (Add_first_char_hash, current) ->
        let new_state = set_current parser_state current in
        let new_state_with_token_start =
          { new_state with token_start = Some (parser_state.offset - 2) }
        in
        handle_state (add_char new_state_with_token_start '#')
      | E (Add_escaped_cr, current) ->
        let new_state = set_current parser_state current in
        handle_state (add_char new_state '\r')
      | E (Push_atom, current) ->
        let new_state = set_current parser_state current in
        let atom, newer_state = push_atom new_state in
        let tokens, newest_state = handle_state newer_state in
        ( if_not_in_sexp_comment
            parser_state
            [ Token_type.String atom |> create_token parser_state ~offset_end_delta:(-1) ]
          @ tokens
        , newest_state )
      | E ((Nop | End_line_comment), current) ->
        handle_state (set_current parser_state current)
      | Error reason -> handle_error reason parser_state char
      | End_block_comment ->
        ( []
        , set_base
            parser_state
            (Base_state.handle_ending_block_comment parser_state.base_state) )
    in
    let tokens, new_t = handle_state parser_state in
    let new_t_flush_errors = { new_t with accumulated_errors = [] } in
    tokens, new_t.accumulated_errors, new_t_flush_errors
  ;;

  let rec process_eoi_with_whole_state parser_state =
    match Automaton.transition_eoi (current parser_state) with
    | Eoi_check ->
      ( []
      , (match Base_state.in_sexp_comment parser_state.base_state with
         | true -> add_error parser_state "Sexp comment without full sexp"
         | false -> parser_state) )
    | E (Add_first_char_hash, current) ->
      let new_state = set_current parser_state current in
      process_eoi_with_whole_state (add_char new_state '#')
    | E (Add_escaped_cr, current) ->
      let new_state = set_current parser_state current in
      process_eoi_with_whole_state (add_char new_state '\r')
    | E (Push_atom, current) ->
      let new_state = set_current parser_state current in
      let atom, newer_state = push_atom new_state in
      let tokens, newest_state = process_eoi_with_whole_state newer_state in
      ( if_not_in_sexp_comment
          parser_state
          [ Token_type.String atom |> create_token parser_state ]
        @ tokens
      , newest_state )
    | E ((Nop | End_line_comment), current) ->
      process_eoi_with_whole_state (set_current parser_state current)
    | Error reason -> handle_error reason parser_state '\000'
  ;;

  let process_eoi parser_state =
    let tokens, new_state = process_eoi_with_whole_state parser_state in
    tokens, new_state.accumulated_errors
  ;;
end

type t = Token_emitting_state.t [@@deriving sexp_of]

let initial = Token_emitting_state.initial_state
let process = Token_emitting_state.process
let process_eoi = Token_emitting_state.process_eoi

let process_string t string =
  let tokens_rev, errors_rev, new_t =
    String.fold string ~init:([], [], t) ~f:(fun (tok_acc, err_acc, t) c ->
      let tokens, errors, t' = process t c in
      tokens :: tok_acc, errors :: err_acc, t')
  in
  let concat_rev x = List.rev x |> List.concat in
  concat_rev tokens_rev, concat_rev errors_rev, new_t
;;
