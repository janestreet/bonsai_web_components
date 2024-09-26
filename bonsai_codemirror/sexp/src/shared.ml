open! Core
open! Codemirror
open Codemirror_incremental_sexp_grammar
open Codemirror_sexp_kernel

let trigger_parse_effect = State.State_effect.define (module Unit)
let parsed_more_effect = State.State_effect.define (module Unit)

let has_effect transaction effect_type =
  List.exists (State.Transaction.effects transaction) ~f:(fun state_effect ->
    State.State_effect.is ~type_:effect_type state_effect)
;;

let trigger_parse_spec =
  State.Transaction_spec.create
    ~effects:[ State.State_effect_type.of_ trigger_parse_effect () ]
    ()
;;

let parsed_more_spec =
  State.Transaction_spec.create
    ~effects:[ State.State_effect_type.of_ parsed_more_effect () ]
    ()
;;

let parsed_more update =
  View.View_update.transactions update
  |> List.exists ~f:(fun transaction -> has_effect transaction parsed_more_effect)
;;

let line_index_of_pos doc pos =
  let number = Text.Text.line_at doc pos |> Text.Line.number in
  (* Codemirror treats line numbers as 1-indexed. *)
  number - 1
;;

module Doc = struct
  type t = Text.Text.t

  module Line_iterator = struct
    type in_progress = Text.Text_iterator.t

    type t =
      | Done
      | In_progress of (in_progress * string)

    let next in_progress =
      let actual_iterator = Text.Text_iterator.next in_progress () in
      if Text.Text_iterator.done_ actual_iterator
      then Done
      else (
        let line = Text.Text_iterator.value actual_iterator in
        In_progress (actual_iterator, line))
    ;;
  end

  let iter_lines t ~start_line =
    (* Codemirror treats line numbers as 1-indexed. *)
    let unstarted_iterator = Text.Text.iter_lines t ~from:(start_line + 1) () in
    (* When you create a text iterator, it doesn't start with the first line; you need
         to first call [next] on it.

         Demo:
         https://codemirror.net/try/?c=aW1wb3J0IHtiYXNpY1NldHVwLCBFZGl0b3JWaWV3fSBmcm9tICJjb2RlbWlycm9yIgoKZnVuY3Rpb24gYnVpbGRMaW5lcyhuKSB7CiAgcmV0dXJuIEFycmF5LmZyb20oe2xlbmd0aDogbn0sIChfLCBpKSA9PiBgTGluZSAke2kgKyAxfWApLmpvaW4oJ1xuJyk7Cn0KCmxldCB2aWV3ID0gbmV3IEVkaXRvclZpZXcoewogIGRvYzogYnVpbGRMaW5lcygxNSksCiAgZXh0ZW5zaW9uczogWwogICAgYmFzaWNTZXR1cCwKICBdLAogIHBhcmVudDogZG9jdW1lbnQuYm9keQp9KQoKd2luZG93Lml0ZXJBbmRQcmludCA9IGZ1bmN0aW9uKCkgewogIGxldCBpID0gMDsKICBsZXQgaXRlcmF0b3IgPSB2aWV3LnN0YXRlLmRvYy5pdGVyTGluZXMoMSk7CiAgd2hpbGUgKCFpdGVyYXRvci5kb25lKSB7CiAgICBpKz0gMTsKICAgIGNvbnNvbGUubG9nKGksIGl0ZXJhdG9yLnZhbHVlLCBpdGVyYXRvci5saW5lQnJlYWssIGl0ZXJhdG9yLmRvbmUpOwogICAgaXRlcmF0b3IgPSBpdGVyYXRvci5uZXh0KCk7CiAgfQogICAgY29uc29sZS5sb2coaXRlcmF0b3IudmFsdWUsIGl0ZXJhdG9yLmxpbmVCcmVhaywgaXRlcmF0b3IuZG9uZSk7Cn0K*)
    Line_iterator.next unstarted_iterator
  ;;
end

(* We use a combination of a state field and a view plugin to simulate a state machine
   that can send actions to itself.

   The state field can react to changes in the content, in which case it clears all
   checkpoints after the edit, and it can parse a bunch of lines and set a checkpoint.

   The view plugin just repeatedly schedules "parse some lines" actions, until the
   entire doc has been parsed. It's also responsible for updating the set of diagnostics
   after each parse. *)
module Parse_driver = struct
  type t =
    | Ungrammared of (Doc.t, Lint.Diagnostic.t) Parsing.Ungrammared.t
    | Grammared of ((Doc.t, Lint.Diagnostic.t) Parsing.Grammared.t * Sexp_grammar.grammar)

  let apply_action state action =
    match state with
    | Ungrammared ungrammared ->
      Parsing.Ungrammared.apply_action ungrammared action |> Ungrammared
    | Grammared (parsing, grammar) ->
      let new_parsing = Parsing.Grammared.apply_action parsing action in
      Grammared (new_parsing, grammar)
  ;;

  let diagnostic_of_syntax_error
    { Syntax_error.error; position = { offset_start; offset_end } }
    =
    Lint.Diagnostic.create
      ~from:offset_start
      ~to_:offset_end
      ~severity:Error
      ~message:error
      ()
  ;;

  let start_of_changeset (changes : State.Change_set.t) =
    let start_of_edit = ref None in
    State.Change_set.iter_changes
      changes
      ~f:(fun ~from_a ~to_a:_ ~from_b:_ ~to_b:_ ~inserted:_ ->
        match !start_of_edit with
        | None -> start_of_edit := Some from_a
        | Some stored -> start_of_edit := Some (Int.min stored from_a));
    !start_of_edit
  ;;

  let grammar_facet = State.Facet.define (State.Facet.Config.create ~combine:List.last ())

  let create_state = function
    | None ->
      Ungrammared
        (Parsing.Ungrammared.create ~error_conv:diagnostic_of_syntax_error (module Doc))
    | Some grammar ->
      Grammared
        ( Parsing.Grammared.create
            ~error_conv:diagnostic_of_syntax_error
            (module Doc)
            grammar
        , grammar )
  ;;

  let update_state_field old_state_maybe_reset transaction =
    let old_state =
      match State.Transaction.reconfigured transaction with
      | false -> old_state_maybe_reset
      | true ->
        let new_grammar =
          State.Editor_state.facet (State.Transaction.state transaction) grammar_facet
        in
        let old_grammar =
          match old_state_maybe_reset with
          | Ungrammared _ -> None
          | Grammared (_, old_grammar) -> Some old_grammar
        in
        if [%equal: Sexp_grammar.grammar option] old_grammar new_grammar
        then old_state_maybe_reset
        else create_state new_grammar
    in
    let doc = State.Transaction.new_doc transaction in
    match State.Transaction.changes transaction |> start_of_changeset with
    | Some start_offset ->
      apply_action
        old_state
        (Edit_starting_at_nth_line (line_index_of_pos doc start_offset))
    | None ->
      (match has_effect transaction trigger_parse_effect with
       | false -> old_state
       | true -> apply_action old_state (Parse_some_lines doc))
  ;;

  let field =
    State.State_field.define
      ~config:
        (State.State_field_config.create
           ~create:(fun editor_state ->
             create_state (State.Editor_state.facet editor_state grammar_facet))
           ~update:update_state_field
           ~compare:None)
  ;;

  let is_done = function
    | Ungrammared state -> Parsing.Ungrammared.is_done state
    | Grammared (state, _) -> Parsing.Grammared.is_done state
  ;;

  let drive_parse view parse_timeout =
    let editor_state = View.Editor_view.state view in
    match State.Editor_state.field editor_state field with
    | None -> ()
    | Some parse_state ->
      (match is_done parse_state, !parse_timeout with
       | true, Some timeout ->
         Js_of_ocaml.Dom_html.clearTimeout timeout;
         parse_timeout := None
       | false, Some _ | true, None -> ()
       | false, None ->
         let rec parse_repeatedly parse_timeout =
           let timeout =
             (* Requesting the next parse and updating diagnostics can't run synchronously,
      because Codemirror doesn't allow us to dispatch transactions inside of a
      view plugin's [update] function.

      We use [setTimeout] instead of [requestIdleCallback] because we want
      parsing / validation to run as soon as possible, as opposed to waiting
      until the main thread is idle. *)
             Js_of_ocaml.Dom_html.setTimeout
               (fun () ->
                 let editor_state = View.Editor_view.state view in
                 match State.Editor_state.field editor_state field with
                 | None -> parse_timeout := None
                 | Some parse_state ->
                   View.Editor_view.dispatch_specs view trigger_parse_spec;
                   View.Editor_view.dispatch_specs view parsed_more_spec;
                   (match is_done parse_state with
                    | true -> parse_timeout := None
                    | false -> parse_repeatedly parse_timeout))
               0.
           in
           parse_timeout := Some timeout
         in
         parse_repeatedly parse_timeout)
  ;;

  let drive_parse =
    View.View_plugin.extension
      (View.View_plugin.define
         ~create:(fun view ->
           let parse_timeout = ref None in
           (* If this plugin is reconfigured, but the doc doesn't change, there might not
              be a view update that triggers parsing to restart, so we do so explicitly. *)
           let start_parsing_on_create =
             Js_of_ocaml.Dom_html.setTimeout (fun () -> drive_parse view parse_timeout) 0.
           in
           ignore (start_parsing_on_create : Js_of_ocaml.Dom_html.timeout_id_safe);
           { update = Some (fun _ -> drive_parse view parse_timeout); custom_state = () })
         ())
  ;;

  let grammared grammar =
    (* This [Obj.magic] is safe, because this is a facet that we've defined, and will only
       ever be accessed from OCaml code. *)
    let grammar_conversion = With_conversion.create ~t_to_js:Obj.magic grammar in
    State.Extension.of_list
      [ State.State_field.extension field
      ; drive_parse
      ; State.Facet.of_ grammar_facet grammar_conversion
      ]
  ;;

  let ungrammared =
    State.Extension.of_list [ State.State_field.extension field; drive_parse ]
  ;;

  let errors_unsorted editor_state =
    match State.Editor_state.field editor_state field with
    | None -> []
    | Some (Ungrammared state) -> Parsing.Ungrammared.errors_unsorted state
    | Some (Grammared (state, _)) -> Parsing.Grammared.errors_unsorted state
  ;;

  let adhoc_tokenization editor_state ~starting_at_line ~until_line =
    let doc = State.Editor_state.doc editor_state in
    match State.Editor_state.field editor_state field with
    | None -> None
    | Some (Ungrammared state) ->
      Parsing.Ungrammared.adhoc_tokenization state ~starting_at_line ~until_line ~doc
    | Some (Grammared (state, _)) ->
      Parsing.Grammared.adhoc_tokenization state ~starting_at_line ~until_line ~doc
  ;;
end
