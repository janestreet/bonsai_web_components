open! Core
module Token = Tokenizer.Token

type error_builder = Tokens_for_error.t -> Lint_error.t [@@deriving sexp_of]

module Error_emission = struct
  type t =
    | Emit_when_error_ends of (error_builder * Tokens_for_error.t)
    | Emitted_when_detected
  [@@deriving sexp_of]
end

module Error_ends = struct
  type t =
    | On_exit_of_start_sexp
    | Before_end_of_list
  [@@deriving sexp_of]
end

module Record_state = struct
  type t =
    { original_record : Sexp_grammar.record
    ; used_fields : String.Set.t
    ; pos_in_field : [ `Outside | `Beginning | `End ]
    }
  [@@deriving sexp_of]
end

type intermediate_t =
  (* Consumes a sexp corresponding to the given grammar. *)
  | Full_grammar of Sexp_grammar.grammar
  (* Consumes the inside of a list (e.g. "1 3 2 4"). Does not consume
       the ending parenthesis of a list, since record arguments use this too. *)
  | Inside_list of Sexp_grammar.list_grammar
  (* Consumes the inside of a record (e.g. "(foo 5) (bar 7)").  *)
  | Inside_record of Record_state.t
  (* Consumes the inside and end of an option, e.g. "Some 5)", "5)", ")"

     Because `(X)` parses as `Some X`, we need to store whether we've seen a token that's
     the string `Some`, because we don't yet know if we've parsed `(Some)` or `(Some X)`.
  *)
  | In_option of
      { payload : Sexp_grammar.grammar
      ; some_token : Token.t option
      }
  | Consume_variant_constructor of Sexp_grammar.variant
  | Consume_variant_payload of Sexp_grammar.list_grammar
  (* [Consume_no_more_args] is like [Inside_list Empty], except that:
       - Errors apply to the contents starting at the error token, not to the whole list.
       - The error message is customizable.  *)
  | Consume_no_more_args of error_builder
  (* Consumes until any of the listed `t`s are satisfied. If none are satisfied,
       defaults to the backup `t`. *)
  | In_union of
      { ts : t list
      ; backup_t : t
      ; start_token : Token.t
      }
  | In_error of
      { start_token : Token.t
      ; error_ends : Error_ends.t
      ; error_emission : Error_emission.t
      }
  (* Consumes a single ). *)
  | Consume_rparen
  (* Pops the topmost [defns] from the stack.
     Also, removes any tyvars registered by the tycon, the names of which are the payload
     of this variant. *)
  | Remove_context of string list
[@@deriving sexp_of]

(* A grammar is added for approximately each level of sexp nesting. *)
and t =
  { stack : intermediate_t list
  ; defns_context : Sexp_grammar.defn list list
  ; tyvars_context : Tyvars_context.t
  ; tokens_since_last_lparen : Tokens_for_error.t option
  ; last_tok_parsed : Token.t option
  }
[@@deriving sexp_of]

let many_any = Inside_list (Many (Any ""))

let create grammar =
  { stack = [ Full_grammar grammar ]
  ; tyvars_context = Tyvars_context.empty
  ; defns_context = []
  ; tokens_since_last_lparen = None
  ; last_tok_parsed = None
  }
;;

let is_done t =
  not
    (List.exists t.stack ~f:(function
      | Remove_context _ | Inside_list Empty | Inside_list (Many _) | In_error _ -> false
      | _ -> true))
;;

let end_of_input_errors t =
  let rec loop stack acc =
    match stack with
    | [] -> acc
    | Remove_context _ :: rest -> loop rest acc
    | In_error { error_emission; _ } :: rest ->
      (match error_emission with
       | Emitted_when_detected -> loop rest acc
       | Emit_when_error_ends (builder, tokens) ->
         let error = builder tokens in
         loop rest (error :: acc))
    | Full_grammar _ :: _
    | Inside_list _ :: _
    | Inside_record _ :: _
    | In_option _ :: _
    | Consume_variant_constructor _ :: _
    | Consume_variant_payload _ :: _
    | Consume_no_more_args _ :: _
    | In_union _ :: _
    | Consume_rparen :: _ ->
      (match t.last_tok_parsed with
       | None ->
         Lint_error.at_pos
           "No sexp was provided. Sexp string must contain non-whitespace characters."
           ~pos:0
       | Some last_tok ->
         let closing_parens_suggestion =
           match last_tok.nesting_level with
           | 0 -> ""
           | n -> [%string " Did you forget %{n#Int} closing parentheses?"]
         in
         Lint_error.at_pos
           ("Unexpected end of input." ^ closing_parens_suggestion)
           ~pos:(last_tok.pos.offset_end - 1))
      :: acc
  in
  List.rev (loop t.stack [])
;;

let rec strip_tags = function
  | Sexp_grammar.Tag { grammar; _ } -> strip_tags grammar
  | No_tag res -> res
;;

let record_arg_grammar = function
  | Sexp_grammar.Empty -> Ok None
  | Cons (grammar, Empty) -> Ok (Some grammar)
  | invalid_grammar ->
    Or_error.error_s
      [%message
        "Invalid sexp grammar: record fields must take exactly zero or one args."
          (invalid_grammar : Sexp_grammar.list_grammar)]
;;

module Error_builders = struct
  let expected_atom atom_type : error_builder =
    fun tokens ->
    let atom_name =
      match atom_type with
      | Sexp_grammar.Bool -> Ok "a bool"
      | Char -> Ok "a char"
      | Float -> Ok "a float"
      | Integer -> Ok "an int"
      | String -> Ok "a string"
      | _ -> Or_error.error_string "INTERNAL ERROR: atom_type was not an atom_type."
    in
    let corrections =
      match Tokens_for_error.tokens_maybe_trunc tokens with
      | [ { Token.token_type = Lparen; _ }
        ; { token_type = String correction; _ }
        ; { token_type = Rparen; _ }
        ] -> [ { Correction.correction; kind = None } ]
      | _ -> []
    in
    match atom_name with
    | Ok atom_name ->
      Lint_error.of_tokens ~corrections [%string "Expected %{atom_name}."] tokens
    | Error error -> Lint_error.of_tokens (Error.to_string_hum error) tokens
  ;;

  let maybe_spellcheck ~possible_corrections error : error_builder =
    fun tokens ->
    match Tokens_for_error.tokens_maybe_trunc tokens with
    | [ { Token.token_type = String s; _ } ] ->
      (match Correction.spellcheck ~possible_corrections s with
       | [] -> Lint_error.of_tokens error tokens
       | corrections -> Lint_error.of_tokens ~corrections error tokens)
    | _ -> Lint_error.of_tokens error tokens
  ;;

  let numeric_value_contains_comma ~value ~default_error_description ~t_of_string_opt =
    match String.contains value ',' with
    | false -> Lint_error.of_tokens default_error_description
    | true ->
      let no_commas_value = String.substr_replace_all value ~pattern:"," ~with_:"" in
      (match t_of_string_opt no_commas_value with
       | None -> Lint_error.of_tokens default_error_description
       | Some _ ->
         Lint_error.of_tokens
           "Numerical values cannot contain commas."
           ~corrections:[ { correction = no_commas_value; kind = None } ])
  ;;

  let expected_list list_fields : error_builder =
    fun tokens ->
    let error =
      match list_fields with
      | Sexp_grammar.Fields fields ->
        "Expected a record that looks like "
        ^ Grammar_tools.to_string (List (Fields fields))
      | Empty -> "Expected an empty list"
      | Many g -> "Expected a list of " ^ Grammar_tools.to_string g
      | Cons (g, _) ->
        "Expected a list, where the first element is a " ^ Grammar_tools.to_string g
    in
    let corrections =
      match list_fields, Tokens_for_error.tokens_maybe_trunc tokens with
      | Sexp_grammar.Fields _, _ -> []
      | Empty, _ -> [ Correction.other "()" ]
      | Many _, [ { Token.token_type = String s; _ } ] ->
        [ Correction.other [%string "(%{s})"] ]
      | Many _, _ -> []
      | Cons _, _ -> []
    in
    Lint_error.of_tokens ~corrections error tokens
  ;;

  let expected_option option_grammar =
    let inner = Grammar_tools.to_string option_grammar in
    let default_error_message =
      [%string "Expected None or (Some %{inner}) for option type."]
    in
    maybe_spellcheck ~possible_corrections:[ "None" ] default_error_message
  ;;

  let expected_end_of_record_field =
    Lint_error.of_tokens
      "Expected end of record field. All values in a record must be a list of one or two \
       elements, e.g., (is_enabled) or (quantity 5)."
  ;;

  let union_error errors tokens =
    match List.stable_dedup errors ~compare:Lint_error.compare_error_descriptions with
    | [] -> Lint_error.of_tokens "Invalid sexp grammar: empty union" tokens
    | [ error ] -> error
    | first :: _ as errors ->
      let error_description =
        "Please fix one of the following errors:"
        :: List.map errors ~f:(fun e -> "- " ^ e.error_description)
        |> String.concat_lines
        |> String.strip
      in
      { first with error_description }
  ;;

  let expected_empty_list = Lint_error.of_tokens "Expected an empty list."

  let expected_record_field_label { Record_state.original_record; used_fields; _ } =
    let fields =
      List.map original_record.fields ~f:strip_tags
      |> List.filter ~f:(fun field -> not (Set.mem used_fields field.name))
    in
    Lint_error.at_pos_of_first_token
      ~corrections:
        (List.map fields ~f:(fun field ->
           { Correction.correction = field.name; kind = None }))
      "Record fields must start with a label."
  ;;
end

let option_payload_error
  ~(token : Tokenizer.Token.t)
  ~payload_grammar
  ~seen_explicit_some
  errors
  t
  =
  let improve_syntax_error (original : Lint_error.t) =
    match seen_explicit_some with
    | true -> original
    | false ->
      let inner = Grammar_tools.to_string payload_grammar in
      let common = [%string "Expected None or (Some %{inner}) for option type."] in
      let prefix, corrections =
        match token.token_type, original.contents with
        | String _, Tokens tokens ->
          let spellcheck_error =
            Error_builders.maybe_spellcheck ~possible_corrections:[ "Some" ] common tokens
          in
          spellcheck_error.error_description, spellcheck_error.corrections
        | Lparen, _ | Rparen, _ | String _, Single_position _ -> common, []
      in
      let error_description =
        String.concat_lines
          [ prefix
          ; "If you're using the shorthand option format [(X) = Some X], the issue might \
             be that:"
          ; original.error_description
          ]
      in
      { original with error_description; corrections }
  in
  match errors, t.stack with
  | [ error ], rest ->
    [ lazy (improve_syntax_error (force error)) ], { t with stack = rest }
  | ( []
    , In_error
        { error_emission = Emit_when_error_ends (builder, tokens_rev)
        ; start_token
        ; error_ends
        }
      :: Consume_rparen
      :: Consume_no_more_args _
      :: rest ) ->
    (* This catches cases like [((X) Y)]. *)
    let better_builder tokens = improve_syntax_error (builder tokens) in
    ( []
    , { t with
        stack =
          In_error
            { error_emission = Emit_when_error_ends (better_builder, tokens_rev)
            ; start_token
            ; error_ends
            }
          :: Consume_rparen
          :: many_any
          :: rest
      } )
  | _ -> errors, t
;;

module Validate = struct
  let bool = function
    | "true" | "True" | "false" | "False" -> Ok ()
    | value ->
      let correct_binary_value correction =
        Lint_error.of_tokens
          "Bool values must be true or false, not binary."
          ~corrections:[ Correction.other correction ]
        |> Error
      in
      (match value with
       | "0" -> correct_binary_value "false"
       | "1" -> correct_binary_value "true"
       | _ ->
         Error_builders.maybe_spellcheck
           ~possible_corrections:[ "true"; "false" ]
           "Bool values must be true or false."
         |> Error)
  ;;

  let char = function
    | value when String.length value = 1 -> Ok ()
    | _ -> Lint_error.of_tokens "Expected a single character, like a or 1." |> Error
  ;;

  let integer value =
    match Zarith.Z.of_string value with
    | _ -> Ok ()
    | exception _ ->
      (match float_of_string_opt value with
       | Some float_value ->
         Lint_error.of_tokens
           "Integer values cannot be floats."
           ~corrections:[ Correction.other (string_of_int (int_of_float float_value)) ]
         |> Error
       | None ->
         Error_builders.numeric_value_contains_comma
           ~value
           ~default_error_description:"Expected an integer value, like -3 or 10."
           ~t_of_string_opt:int_of_string_opt
         |> Error)
  ;;

  let float value =
    match float_of_string_opt value with
    | Some _ -> Ok ()
    | None ->
      Error_builders.numeric_value_contains_comma
        ~value
        ~default_error_description:"Expected a float value, like -2.0 or 3.14."
        ~t_of_string_opt:float_of_string_opt
      |> Error
  ;;

  (* Given a record and a field name, get the grammar for that field's arguments
     (if applicable) and whether the field was required. *)
  let record_field
    { Record_state.used_fields; original_record = { allow_extra_fields; fields }; _ }
    s
    =
    let fields = List.map fields ~f:strip_tags in
    let field = List.find fields ~f:(fun { name; _ } -> String.equal s name) in
    let field_was_used =
      if Set.mem used_fields s then `Duplicate_field else `First_occurence
    in
    let allow_extra = if allow_extra_fields then `Extra_allowed else `Extra_banned in
    match field, allow_extra, field_was_used with
    | None, `Extra_allowed, _ -> Ok [ many_any ]
    | None, `Extra_banned, _ ->
      let unused_field_names =
        List.map fields ~f:(fun { name; _ } -> name)
        |> List.filter ~f:(fun field -> not (Set.mem used_fields field))
      in
      (match unused_field_names with
       | [] ->
         Lint_error.of_tokens "This record is complete, and does not allow extra fields."
         |> Error
       | _ ->
         Error_builders.maybe_spellcheck
           ~possible_corrections:unused_field_names
           "Invalid field name, and additional fields are not allowed."
         |> Error)
    | Some _, _, `Duplicate_field ->
      Lint_error.of_tokens "This field was already used earlier in this record." |> Error
    | Some { required = false; args; _ }, _, `First_occurence
    | Some { args; _ }, _, `First_occurence ->
      (match record_arg_grammar args with
       | Ok None -> Ok []
       | Ok (Some grammar) -> Ok [ Full_grammar grammar ]
       | Error error -> Error (Lint_error.of_tokens (Error.to_string_hum error)))
  ;;

  let end_of_record { Record_state.original_record = { fields; _ }; used_fields; _ } =
    let required_field_names =
      List.filter_map fields ~f:(fun field ->
        let field = strip_tags field in
        Option.some_if field.required field.name)
    in
    let required_but_not_present_fields =
      List.filter required_field_names ~f:(fun req_field ->
        not (Set.mem used_fields req_field))
    in
    let req_string = String.concat required_but_not_present_fields ~sep:", " in
    match required_but_not_present_fields with
    | [] -> Ok ()
    | _ ->
      Lint_error.at_pos_of_first_token [%string "Missing required fields: %{req_string}"]
      |> Error
  ;;

  let case_insensitive_equal s1 s2 =
    let s1_len = String.length s1 in
    let s2_len = String.length s2 in
    if s1_len <> s2_len
    then
      false
      (* This should never happen, because a 0-length string wouldn't be parsed as a token.
       But writing correct code is a good thing. *)
    else if s1_len = 0
    then true
    else if Char.equal (Char.lowercase s1.[0]) (Char.lowercase s2.[0])
    then (
      let rec loop i =
        if i >= s1_len
        then true
        else if Char.equal s1.[i] s2.[i]
        then loop (i + 1)
        else false
      in
      loop 1)
    else false
  ;;

  let get_variant_clause ({ case_sensitivity; clauses } : Sexp_grammar.variant) s =
    let clauses = List.map clauses ~f:strip_tags in
    let is_equal =
      match case_sensitivity with
      | Case_insensitive -> String.Caseless.equal
      | Case_sensitive -> String.equal
      | Case_sensitive_except_first_character -> case_insensitive_equal
    in
    let clause = List.find clauses ~f:(fun clause -> is_equal clause.name s) in
    match clause with
    | None ->
      let clause_names = List.map clauses ~f:(fun { name; _ } -> name) in
      Error_builders.maybe_spellcheck
        ~possible_corrections:clause_names
        "Unknown variant name."
      |> Error
    | Some { clause_kind; _ } -> Ok clause_kind
  ;;

  let variant_outside_parens variant name =
    match get_variant_clause variant name with
    | Error error -> Error error
    | Ok (List_clause { args }) ->
      let inner = Grammar_tools.arg_to_string args in
      Lint_error.of_tokens
        [%string
          "This variant is not atomic, and requires a list of two elements: the field \
           name and the value, e.g., (%{name} %{inner})"]
      |> Error
    | Ok Atom_clause -> Ok ()
  ;;

  let variant_inside_parens variant s =
    match get_variant_clause variant s with
    | Error error -> Error error
    | Ok Atom_clause ->
      Lint_error.of_tokens
        "This variant is atomic and cannot have parentheses around it, or take an \
         argument."
      |> Error
    | Ok (List_clause { args }) -> Ok args
  ;;
end

let no_error = []

let rec process_token t_without_registering_token (token : Token.t) =
  let toks_since_last_lparen =
    match t_without_registering_token.tokens_since_last_lparen with
    | None -> Tokens_for_error.singleton token
    | Some x -> Tokens_for_error.push x token
  in
  let new_tokens_since_last_lparen =
    let curr = t_without_registering_token.tokens_since_last_lparen in
    match token.token_type, curr with
    | Lparen, _ -> Some (Tokens_for_error.singleton token)
    | _, None -> None
    | _, Some curr -> Some (Tokens_for_error.push curr token)
  in
  let t =
    { t_without_registering_token with
      last_tok_parsed = Some token
    ; tokens_since_last_lparen = new_tokens_since_last_lparen
    }
  in
  let ok stack = no_error, { t with stack } in
  let consume ?(to_consume = token) stack = process_token { t with stack } to_consume in
  let curr_tok_err ~stack builder =
    [ lazy (builder (Tokens_for_error.singleton token)) ], { t with stack }
  in
  let err_multi
    ?(error_ends = Error_ends.On_exit_of_start_sexp)
    ?(start_error_at = `Curr_token)
    ?(emit_error = `At_end_of_error)
    ~stack
    builder
    =
    let tokens_so_far () =
      match start_error_at with
      | `Curr_token -> Tokens_for_error.singleton token
      | `Last_lparen -> toks_since_last_lparen
    in
    let emit_error_now () = [ lazy (builder (tokens_so_far ())) ] in
    match token.token_type with
    | Rparen -> emit_error_now (), process_ignoring_error { t with stack } token
    | String _ | Lparen ->
      let start_token =
        match start_error_at with
        | `Curr_token -> token
        | `Last_lparen -> Tokens_for_error.first_token toks_since_last_lparen
      in
      let curr_error, error_emission =
        match emit_error with
        | `At_end_of_error ->
          [], Error_emission.Emit_when_error_ends (builder, tokens_so_far ())
        | `Now -> emit_error_now (), Emitted_when_detected
      in
      ( curr_error
      , { t with stack = In_error { start_token; error_emission; error_ends } :: stack } )
  in
  let validate_atom ~rest = function
    | Ok () -> ok rest
    | Error builder ->
      let stack =
        match rest with
        | Inside_list (Cons _) :: rest | Consume_no_more_args _ :: rest ->
          many_any :: rest
        | rest -> rest
      in
      curr_tok_err ~stack builder
  in
  match token.token_type, t.stack with
  (* Special cases *)
  | Rparen, Consume_rparen :: rest -> ok rest
  | (Lparen | String _), Consume_rparen :: _ ->
    err_multi
      ~error_ends:Before_end_of_list
      ~stack:t.stack
      (Lint_error.of_tokens "Expected closing parenthesis")
  (* Single-atom cases *)
  | String s, Full_grammar Bool :: rest -> validate_atom ~rest (Validate.bool s)
  | String s, Full_grammar Char :: rest -> validate_atom ~rest (Validate.char s)
  | String s, Full_grammar Float :: rest -> validate_atom ~rest (Validate.float s)
  | String s, Full_grammar Integer :: rest -> validate_atom ~rest (Validate.integer s)
  | String _, Full_grammar String :: rest -> ok rest
  | Lparen, Full_grammar ((Bool | Char | Float | Integer | String) as atom_type) :: rest
    ->
    let rest =
      match rest with
      | Inside_list (Cons (_, Cons _)) :: rest -> rest
      | rest -> rest
    in
    err_multi (Error_builders.expected_atom atom_type) ~stack:(Consume_rparen :: rest)
  | Rparen, Full_grammar ((Bool | Char | Float | Integer | String) as atom_type) :: rest
    ->
    let rest =
      match rest with
      | Inside_list (Cons (_, Cons _)) :: rest | Consume_no_more_args _ :: rest -> rest
      | rest -> rest
    in
    err_multi (Error_builders.expected_atom atom_type) ~stack:rest
  (* Option *)
  | String ("none" | "None"), Full_grammar (Option _) :: rest -> ok rest
  | Lparen, Full_grammar (Option payload) :: rest ->
    ok (In_option { payload; some_token = None } :: rest)
  | (Rparen | String _), Full_grammar (Option grammar) :: rest ->
    err_multi
      ~start_error_at:`Last_lparen
      (Error_builders.expected_option grammar)
      ~stack:rest
  | String ("some" | "Some"), In_option { payload; some_token = None } :: rest ->
    ok (In_option { payload; some_token = Some token } :: rest)
  | (Lparen | String _), In_option { payload; some_token } :: rest ->
    let errors, stack =
      consume
        (Full_grammar payload
         :: Consume_no_more_args
              (Lint_error.of_tokens "Options only accept one argument.")
         :: Consume_rparen
         :: rest)
    in
    option_payload_error
      ~token
      ~payload_grammar:payload
      ~seen_explicit_some:(Option.is_some some_token)
      errors
      stack
  | Rparen, In_option { some_token = None; _ } :: rest ->
    (* This parses as `()`, i.e. `None`. *)
    ok rest
  | Rparen, In_option { some_token = Some some_token; payload; _ } :: rest ->
    consume ~to_consume:some_token (Full_grammar payload :: rest)
  (* Union *)
  | _, Full_grammar (Union grammars) :: rest ->
    consume
      (In_union
         { ts =
             List.map grammars ~f:(fun grammar ->
               { t with stack = [ Full_grammar grammar ] })
         ; backup_t = { t with stack = rest }
         ; start_token = token
         }
       :: rest)
  | _, In_union { ts; backup_t; start_token } :: rest ->
    let errors_and_ts = List.map ts ~f:(fun t -> process_token t token) in
    let ok_ts, in_error, already_errored =
      List.partition3_map errors_and_ts ~f:(function
        | [], { stack = In_error { error_emission = Emitted_when_detected; _ } :: _; _ }
          -> `Trd []
        | ( []
          , { stack =
                In_error { error_emission = Emit_when_error_ends (builder, tokens); _ }
                :: _
            ; _
            } ) -> `Snd (builder, tokens)
        | [], t -> `Fst t
        | errs, _ -> `Trd errs)
    in
    let t_done = List.filter ok_ts ~f:is_done in
    (match t_done, ok_ts with
     | res :: _, _ | _, [ res ] -> ok (res.stack @ rest)
     | _, [] ->
       let errors =
         List.map in_error ~f:(fun (builder, tokens) -> builder tokens)
         :: List.map already_errored ~f:(List.map ~f:force)
       in
       let consume_rparen_if_needed =
         match start_token.token_type with
         | Lparen -> [ Consume_rparen ]
         | _ -> []
       in
       let union_tokens =
         match List.hd in_error with
         | None -> Tokens_for_error.singleton start_token
         | Some (_, tokens) -> tokens
       in
       ( [ lazy (Error_builders.union_error (List.concat errors) union_tokens) ]
       , { backup_t with
           stack =
             (In_error
                { start_token
                ; error_emission = Emitted_when_detected
                ; error_ends = On_exit_of_start_sexp
                }
              :: consume_rparen_if_needed)
             @ backup_t.stack
         } )
     | _, _ -> ok (In_union { ts = ok_ts; backup_t; start_token } :: rest))
  | _, In_error { start_token; error_emission = old_error_emission; error_ends } :: rest
    ->
    let error_emission, build_errors =
      match old_error_emission with
      | Emitted_when_detected -> old_error_emission, fun ?exclude_last_token:_ () -> []
      | Emit_when_error_ends (builder, old_tokens) ->
        let new_tokens = Tokens_for_error.push old_tokens token in
        let build_errors ?(exclude_last_token = false) () =
          let tokens = if exclude_last_token then old_tokens else new_tokens in
          [ lazy (builder tokens) ]
        in
        Emit_when_error_ends (builder, new_tokens), build_errors
    in
    let start_level = start_token.nesting_level in
    (match error_ends, token.token_type, start_token.token_type with
     | On_exit_of_start_sexp, Rparen, Lparen when token.nesting_level = start_level ->
       build_errors (), process_ignoring_error { t with stack = rest } token
     | On_exit_of_start_sexp, _, _ when token.nesting_level = start_level ->
       ( build_errors ~exclude_last_token:true ()
       , process_ignoring_error { t with stack = rest } token )
     | On_exit_of_start_sexp, Rparen, (String _ | Rparen)
       when token.nesting_level = start_level - 1 ->
       ( build_errors ~exclude_last_token:true ()
       , process_ignoring_error { t with stack = rest } token )
     | Before_end_of_list, Rparen, _ when token.nesting_level = start_level - 1 ->
       ( build_errors ~exclude_last_token:true ()
       , process_ignoring_error { t with stack = rest } token )
     | On_exit_of_start_sexp, _, _ | Before_end_of_list, _, _ ->
       ok (In_error { start_token; error_emission; error_ends } :: rest))
  (* Any *)
  | Lparen, Full_grammar (Any _) :: rest -> ok (many_any :: Consume_rparen :: rest)
  | Rparen, Full_grammar (Any _) :: rest -> ok rest
  | String _, Full_grammar (Any _) :: rest -> ok rest
  (* Tagged *)
  | _, Full_grammar (Tagged { grammar; _ }) :: rest ->
    consume (Full_grammar grammar :: rest)
  (* Lazy *)
  | _, Full_grammar (Lazy lazy_grammar) :: rest ->
    consume (Full_grammar (Lazy.force lazy_grammar) :: rest)
  (* Lists *)
  | Lparen, Full_grammar (List list) :: rest ->
    ok (Inside_list list :: Consume_rparen :: rest)
  | (String _ | Rparen), Full_grammar (List list_contents) :: rest ->
    err_multi
      ~emit_error:`Now
      ~error_ends:Before_end_of_list
      (Error_builders.expected_list list_contents)
      ~stack:rest
  | Rparen, (Inside_list (Empty | Many _) | Consume_no_more_args _) :: rest ->
    consume rest
  | _, Inside_list (Cons (hd, Empty)) :: rest ->
    consume
      (Full_grammar hd
       :: Consume_no_more_args
            (Lint_error.of_tokens
               "Unexpected argument; this sexp has already received all its arguments.")
       :: rest)
  | _, Inside_list (Cons (hd, tl)) :: rest ->
    consume (Full_grammar hd :: Inside_list tl :: rest)
  | (Lparen | String _), Inside_list (Many grammar) :: rest ->
    consume (Full_grammar grammar :: Inside_list (Many grammar) :: rest)
  | (Lparen | String _), Consume_no_more_args builder :: rest ->
    err_multi ~error_ends:Before_end_of_list ~stack:rest builder
  | Lparen, Inside_list Empty :: rest ->
    err_multi
      ~start_error_at:`Last_lparen
      Error_builders.expected_empty_list
      ~stack:(Consume_rparen :: rest)
  | String _, Inside_list Empty :: rest ->
    err_multi ~start_error_at:`Last_lparen Error_builders.expected_empty_list ~stack:rest
  | _, Inside_list (Fields original_record) :: rest ->
    consume
      (Inside_record
         { original_record; used_fields = String.Set.empty; pos_in_field = `Outside }
       :: rest)
  (* Records *)
  | Lparen, Inside_record ({ pos_in_field = `Outside; _ } as record) :: rest ->
    ok (Inside_record { record with pos_in_field = `Beginning } :: rest)
  | String _, Inside_record { pos_in_field = `Outside; _ } :: _ ->
    curr_tok_err ~stack:t.stack (Lint_error.of_tokens "Record field expected")
  | (Lparen | Rparen), Inside_record ({ pos_in_field = `Beginning; _ } as record) :: rest
    ->
    err_multi
      ~emit_error:`Now
      ~stack:(Inside_record { record with pos_in_field = `End } :: rest)
      (Error_builders.expected_record_field_label record)
  | (Lparen | String _), Inside_record { pos_in_field = `End; _ } :: _ ->
    err_multi ~emit_error:`Now ~stack:t.stack Error_builders.expected_end_of_record_field
  | Rparen, Inside_record ({ pos_in_field = `Outside; _ } as record) :: rest ->
    (match Validate.end_of_record record with
     | Ok () -> consume rest
     | Error err -> err_multi err ~stack:rest)
  | Rparen, Inside_record ({ pos_in_field = `End; _ } as record) :: rest ->
    ok (Inside_record { record with pos_in_field = `Outside } :: rest)
  | String s, Inside_record ({ pos_in_field = `Beginning; _ } as record) :: rest ->
    (match Validate.record_field record s with
     | Ok next ->
       ok
         (next
          @ (Inside_record
               { record with
                 used_fields = Set.add record.used_fields s
               ; pos_in_field = `End
               }
             :: rest))
     | Error err ->
       err_multi
         ~emit_error:`Now
         ~error_ends:Before_end_of_list
         err
         ~stack:(Inside_record { record with pos_in_field = `End } :: rest))
  (* Variants *)
  | Lparen, Full_grammar (Variant variant) :: rest ->
    ok (Consume_variant_constructor variant :: rest)
  | String s, Full_grammar (Variant variant) :: rest ->
    (match Validate.variant_outside_parens variant s with
     | Ok () -> ok rest
     | Error e -> curr_tok_err ~stack:rest e)
  | Rparen, Full_grammar (Variant _) :: rest ->
    err_multi
      ~emit_error:`Now
      ~start_error_at:`Last_lparen
      ~stack:rest
      (Lint_error.of_tokens "Expected a variant, got a closing paren.")
  | Lparen, Consume_variant_constructor _ :: rest ->
    err_multi
      ~emit_error:`Now
      ~stack:(Consume_rparen :: Consume_rparen :: rest)
      (Lint_error.of_tokens "Expected variant constructor, got an opening paren.")
  | Rparen, Consume_variant_constructor _ :: rest ->
    err_multi
      ~emit_error:`Now
      ~stack:(Consume_rparen :: rest)
      (Lint_error.of_tokens "Expected variant constructor, got a closing paren.")
  | String s, Consume_variant_constructor variant :: rest ->
    (match Validate.variant_inside_parens variant s with
     | Ok next -> ok (Consume_variant_payload next :: rest)
     | Error err ->
       err_multi
         ~emit_error:`Now
         ~error_ends:Before_end_of_list
         ~stack:(Consume_rparen :: rest)
         err)
  | Rparen, Consume_variant_payload ((Many _ | Empty) as grammar) :: rest
  | (String _ | Lparen), Consume_variant_payload grammar :: rest ->
    let next =
      match grammar with
      | Cons (next, Empty) ->
        [ Full_grammar next
        ; Consume_no_more_args
            (Lint_error.of_tokens
               "Extra argument. This variant accepts only one payload, which you have \
                already provided.")
        ]
      | _ -> [ Inside_list grammar ]
    in
    consume (next @ (Consume_rparen :: rest))
  | Rparen, Consume_variant_payload grammar :: rest ->
    let inner = Grammar_tools.arg_to_string grammar in
    curr_tok_err
      ~stack:rest
      (Lint_error.at_pos_of_first_token
         [%string "This variant requires a payload of type %{inner}."])
  (* Recursive types *)
  | _, Full_grammar (Tycon (name, args, defns)) :: rest ->
    (match List.find defns ~f:(fun defn -> String.equal defn.tycon name) with
     | None ->
       err_multi
         ~emit_error:`Now
         ~stack:rest
         (Lint_error.at_pos_of_first_token
            [%string
              "Invalid grammar. Recursive type/constructor name not found: %{name}"])
     | Some { tyvars; grammar; _ } ->
       (match Tyvars_context.register_many t.tyvars_context tyvars args with
        | Error error ->
          err_multi
            ~emit_error:`Now
            ~stack:rest
            (Lint_error.at_pos_of_first_token (Error.to_string_hum error))
        | Ok new_tyvars_context ->
          let new_defns_context = defns :: t.defns_context in
          process_token
            { t with
              stack = Full_grammar grammar :: Remove_context tyvars :: rest
            ; defns_context = new_defns_context
            ; tyvars_context = new_tyvars_context
            }
            token))
  | _, Remove_context tyvars_to_remove :: rest ->
    let new_defns =
      match t.defns_context with
      | [] -> Or_error.error_s [%message "Internal error: defns_context is empty"]
      | _ :: tl -> Ok tl
    in
    let new_tyvars = Tyvars_context.unregister_many t.tyvars_context tyvars_to_remove in
    (match Or_error.both new_defns new_tyvars with
     | Ok (new_defns, new_tyvars) ->
       process_token
         { t with stack = rest; defns_context = new_defns; tyvars_context = new_tyvars }
         token
     | Error error ->
       err_multi
         ~stack:rest
         ~emit_error:`Now
         (Lint_error.at_pos_of_first_token (Error.to_string_hum error)))
  | _, Full_grammar (Tyvar id) :: rest ->
    (match Tyvars_context.resolve t.tyvars_context id with
     | Some grammar -> consume (Full_grammar grammar :: rest)
     | None ->
       err_multi
         ~stack:rest
         ~emit_error:`Now
         (Lint_error.at_pos_of_first_token
            [%string "Invalid grammar. No type variable in context called %{id}"]))
  | _, Full_grammar (Recursive (name, args)) :: rest ->
    (match t.defns_context with
     | defns :: _ -> consume (Full_grammar (Tycon (name, args, defns)) :: rest)
     | [] ->
       curr_tok_err
         ~stack:rest
         (Lint_error.of_tokens
            "Invalid grammar. Recursive type without surrounding tycon"))
  (* General error case *)
  | Rparen, [] ->
    curr_tok_err (Lint_error.at_pos_of_first_token "Unmatched parenthesis") ~stack:[]
  | (Lparen | String _), [] ->
    err_multi
      ~emit_error:`Now
      ~stack:[]
      (Lint_error.at_pos_of_first_token
         "Expected only one S-expression, but parsed the start of a second S-expression.")

and process_ignoring_error t token = process_token t token |> snd

let process state token =
  let lazy_errors, new_state = process_token state token in
  let errors = List.map lazy_errors ~f:force in
  errors, new_state
;;

module For_testing = struct
  let stack { stack; _ } = Sexp.to_string_hum [%sexp (stack : intermediate_t list)]

  let assert_nothing_too_crazy { stack; defns_context; tyvars_context; _ } =
    let stack_size = List.length stack in
    if stack_size > 1_000
    then print_s [%message "Suspiciously large stack" (stack_size : int)];
    let defns_context_size = List.length defns_context in
    if defns_context_size > 50
    then
      print_s
        [%message
          "Suspiciously large defns_context. Consider implementing LRE?"
            (defns_context_size : int)];
    let max_tyvars_size = Tyvars_context.For_testing.max_size tyvars_context in
    if max_tyvars_size > 50
    then
      print_s
        [%message
          "Suspiciously large tyvars_context. Consider implementing LRE?"
            (max_tyvars_size : int)]
  ;;
end
