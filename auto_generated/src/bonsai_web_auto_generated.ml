open! Core
open! Bonsai_web
module View = Bonsai_web_legacy_view
open Bonsai.Let_syntax
module N = Vdom.Node
module A = Vdom.Attr
module Form = Bonsai_web_form.With_automatic_view
module Form2 = Bonsai_web_form.With_manual_view
module E = Form.Elements

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

type form_transformer =
  Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t
  -> recurse:
       (Sexp_grammar.grammar Bonsai.t -> Bonsai.graph @ local -> Sexp.t Form.t Bonsai.t)
  -> Bonsai.graph @ local
  -> Sexp.t Form.t Bonsai.t

let error_hint form (local_ graph) =
  let form = form graph in
  Form.Dynamic.error_hint form graph
;;

module Customization = struct
  type 'a t =
    { apply_to_tag : key:string -> value:Sexp.t -> bool
    ; on_match : 'a
    }

  let transform_form'
    (type a)
    (module M : Sexpable with type t = a)
    ~apply_to_tag
    (on_match :
      Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t
      -> recurse:
           (Sexp_grammar.grammar Bonsai.t
            -> Bonsai.graph @ local
            -> Sexp.t Form.t Bonsai.t)
      -> Bonsai.graph @ local
      -> a Form.t Bonsai.t)
    =
    let transform grammar ~recurse (local_ graph) =
      let%arr.Bonsai on_match = on_match grammar ~recurse graph in
      Form.project on_match ~parse_exn:[%sexp_of: M.t] ~unparse:[%of_sexp: M.t]
    in
    { apply_to_tag; on_match = transform }
  ;;

  let transform_form
    ~apply_to_tag
    (on_match :
      Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t
      -> recurse:
           (Sexp_grammar.grammar Bonsai.t
            -> Bonsai.graph @ local
            -> Sexp.t Form.t Bonsai.t)
      -> Bonsai.graph @ local
      -> Sexp.t Form.t Bonsai.t)
    =
    transform_form' (module Sexp) ~apply_to_tag on_match
  ;;

  let constant_form
    (type a)
    (module M : Sexpable with type t = a)
    ~apply_to_tag
    (on_match : Bonsai.graph @ local -> a Form.t Bonsai.t)
    =
    transform_form' (module M) ~apply_to_tag (fun _ ~recurse:_ -> on_match)
  ;;

  let applies t (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag) =
    t.apply_to_tag ~key:with_tag.key ~value:with_tag.value
  ;;

  let apply t = t.on_match

  module Defaults = struct
    module Form = struct
      let transform_key_data_pair
        (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t)
        ~recurse
        (local_ graph)
        =
        let grammar =
          let%arr with_tag in
          with_tag.grammar
        in
        match%sub grammar with
        | List (Many (List (Cons (first, Cons (second, Empty))))) ->
          let pair_form (local_ graph) =
            let first =
              error_hint
                (fun (local_ graph) ->
                  let form = recurse first graph in
                  let%arr form in
                  let view =
                    Form.View.set_label (Vdom.Node.text "Key") (Form.view form)
                  in
                  Form.Expert.create ~view ~value:(Form.value form) ~set:(Form.set form))
                graph
            in
            let second =
              error_hint
                (fun (local_ graph) ->
                  let form = recurse second graph in
                  let%arr form in
                  let view =
                    Form.View.set_label (Vdom.Node.text "Data") (Form.view form)
                  in
                  Form.Expert.create ~view ~value:(Form.value form) ~set:(Form.set form))
                graph
            in
            let%arr first and second in
            let view = Form.View.tuple [ Form.view first; Form.view second ] in
            let value =
              match Or_error.both (Form.value first) (Form.value second) with
              | Ok (first, second) -> Ok (Sexp.List [ first; second ])
              | Error _ as err -> err
            in
            let set = function
              | Sexp.List [ first_val; second_val ] ->
                (* The order of these sets is important and enables optimization! *)
                Effect.Many [ Form.set second second_val; Form.set first first_val ]
              | _ -> Effect.Ignore
            in
            Form.Expert.create ~view ~value ~set
          in
          Bonsai.map
            (Form.Elements.Multiple.list pair_form graph)
            ~f:
              (Form.project
                 ~parse_exn:(fun l -> Sexp.List l)
                 ~unparse:(function
                   | Sexp.List l -> l
                   | Sexp.Atom _ -> []))
        | _ -> recurse grammar graph
      ;;

      let assoc_key_value_labels =
        transform_form
          ~apply_to_tag:(fun ~key ~value:_ ->
            String.equal key Sexplib0.Sexp_grammar.assoc_tag)
          transform_key_data_pair
      ;;

      let transform_multiple_button_name
        (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t)
        ~(recurse :
            Sexp_grammar.grammar Bonsai.t
            -> Bonsai.graph @ local
            -> Sexp.t Form.t Bonsai.t)
        (local_ graph)
        =
        let%sub { grammar; value; key = _ } = with_tag in
        let form = recurse grammar graph in
        let text =
          match%arr value with
          | Sexp.Atom s -> s
          | value -> Sexp.to_string_hum value
        in
        let%arr form and text in
        let view =
          let view = Form.view form in
          match view.view with
          | List ({ append_item = Append_info { append; text = None }; _ } as t) ->
            { view with
              view =
                List { t with append_item = Append_info { append; text = Some text } }
            }
          | List { append_item = Append_view _; _ }
          | List { append_item = Append_info { text = Some _; _ }; _ }
          | Empty | Collapsible _ | Raw _ | Record _ | Variant _ | Tuple _ | Option _ ->
            view
        in
        Form.Expert.create ~value:(Form.value form) ~set:(Form.set form) ~view
      ;;

      let transform_multiple_button_name =
        transform_form
          ~apply_to_tag:(fun ~key ~value:_ -> String.equal key "grammar.add_element_text")
          transform_multiple_button_name
      ;;

      (* Without override, these would all be string text boxes *)
      let nice_time_ns ~allow_updates_when_focused =
        constant_form
          (module Time_ns.Alternate_sexp)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Alternate_sexp.t"))
          (Form.Elements.Date_time.datetime_local ~allow_updates_when_focused ())
      ;;

      let nice_time_of_day ~allow_updates_when_focused =
        constant_form
          (module Time_ns.Ofday)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Time_ns.Ofday.t"))
          (Form.Elements.Date_time.time ~allow_updates_when_focused ())
      ;;

      let nice_date ~allow_updates_when_focused =
        constant_form
          (module Date)
          ~apply_to_tag:(fun ~key ~value ->
            String.equal key Sexplib0.Sexp_grammar.type_name_tag
            && Sexp.equal value ([%sexp_of: string] "Core.Date.t"))
          (Form.Elements.Date_time.date ~allow_updates_when_focused ())
      ;;

      let all ?(allow_updates_when_focused = `Always) () =
        [ assoc_key_value_labels
        ; nice_time_ns ~allow_updates_when_focused:`Never
        ; nice_time_of_day ~allow_updates_when_focused
        ; nice_date ~allow_updates_when_focused
        ; transform_multiple_button_name
        ]
      ;;
    end
  end
end

module Style =
  [%css
  stylesheet
    {|
      .with_whitespace {
        white-space: pre-wrap;
      }

      .record_field_name {
        font-weight: bold;
      }

      .error {
        font-weight: bold;
        border: solid 2px red;
      }

      .override_showing {
        color: black;
      }

      .override_hidden {
        color: gray;
      }

      .override_text {
        text-align: center;
        font-weight: bold;
        cursor: pointer;
      }

      pre {
        margin: 0;
      }

      .inline_padding {
        padding-inline: 5px;
      }

      .bold_text {
        font-weight: 700;
      }

      .scrollable_tooltip {
        overflow-y: auto;
        /* arbitrary value from eyeballing */
        max-height: 14rem;
      }
    |}]

let project_to_sexp
  (type a)
  (module M : Sexpable with type t = a)
  (form : Bonsai.graph @ local -> a Form.t Bonsai.t)
  (local_ graph)
  =
  let%arr.Bonsai form = form graph in
  Form.project form ~parse_exn:M.sexp_of_t ~unparse:M.t_of_sexp
;;

let maybe_set_tooltip ~tooltip_format_of_doc_string doc view =
  match doc with
  | Some str -> Form.View.set_tooltip (tooltip_format_of_doc_string str) view
  | None -> view
;;

let combine_views views =
  match views with
  | [] -> Form.View.empty
  | [ single ] -> single
  | _ -> Form.View.tuple views
;;

module Grammar = struct
  include Sexp_grammar

  module T = struct
    type t = grammar [@@deriving sexp_of, compare]
  end

  include T
  include Comparator.Make (T)
end

type generated_form = Bonsai.graph @ local -> Sexp.t Form.t Bonsai.t

type folded_grammar =
  { grammar : Sexp_grammar.grammar
  ; form : generated_form
  ; find_form : Sexp_grammar.grammar -> generated_form option
  ; variant : folded_variant option
  }

and folded_variant =
  { case_sensitivity : Sexp_grammar.case_sensitivity
  ; clauses : folded_clause list
  }

and folded_clause =
  { name : string
  ; args : folded_list_grammar option
  ; tags : (string * Sexp.t) list
  }

and folded_list_grammar =
  | Empty
  | Cons of folded_grammar * folded_list_grammar
  | Many of folded_grammar
  | Fields of
      { fields : folded_field list
      ; allow_extra_fields : bool
      }

and folded_field =
  { name : string
  ; required : bool
  ; args : folded_list_grammar
  ; tags : (string * Sexp.t) list
  }

let rec find_form_in_list_grammar grammar query =
  match grammar with
  | Empty -> None
  | Cons (grammar, rest) ->
    (match grammar.find_form query with
     | Some _ as form -> form
     | None -> find_form_in_list_grammar rest query)
  | Many grammar -> grammar.find_form query
  | Fields { fields; _ } ->
    List.find_map fields ~f:(fun { args; _ } -> find_form_in_list_grammar args query)
;;

let find_doc_tag tags =
  List.find_map (List.rev tags) ~f:(fun (key, value) ->
    if String.equal key Ppx_sexp_conv_lib.Sexp_grammar.doc_comment_tag
    then
      Some
        (match value with
         | Sexp.Atom str -> str
         | List _ -> Sexp.to_string_hum value)
    else None)
;;

let rec sexp_grammar_of_folded_list = function
  | Empty -> Sexp_grammar.Empty
  | Cons (grammar, rest) ->
    Sexp_grammar.Cons (grammar.grammar, sexp_grammar_of_folded_list rest)
  | Many grammar -> Sexp_grammar.Many grammar.grammar
  | Fields { fields; allow_extra_fields } ->
    let fields =
      List.map fields ~f:(fun { name; required; args; tags } ->
        let field =
          { Sexp_grammar.name; required; args = sexp_grammar_of_folded_list args }
        in
        List.fold_right
          tags
          ~init:(Sexp_grammar.No_tag field)
          ~f:(fun (key, value) grammar -> Sexp_grammar.Tag { key; value; grammar }))
    in
    Sexp_grammar.Fields { fields; allow_extra_fields }
;;

let sexp_grammar_of_folded_variant { case_sensitivity; clauses } =
  let clauses =
    List.map clauses ~f:(fun { name; args; tags } ->
      let clause_kind =
        match args with
        | None -> Sexp_grammar.Atom_clause
        | Some args ->
          Sexp_grammar.List_clause { args = sexp_grammar_of_folded_list args }
      in
      let clause = { Sexp_grammar.name; clause_kind } in
      List.fold_right
        tags
        ~init:(Sexp_grammar.No_tag clause)
        ~f:(fun (key, value) grammar -> Sexp_grammar.Tag { key; value; grammar }))
  in
  Sexp_grammar.Variant { case_sensitivity; clauses }
;;

let constant_length_tuple_form ~grammar_form grammars (local_ graph) =
  let all_forms =
    Bonsai.all (List.map grammars ~f:(fun grammar -> grammar_form grammar graph))
  in
  let%arr all_forms in
  let value =
    Or_error.all (List.map all_forms ~f:Form.value)
    |> Or_error.map ~f:(fun values -> Sexp.List values)
  in
  let view =
    match all_forms with
    | [ form ] -> Form.view form
    | _ ->
      Form.View.tuple
        (List.mapi all_forms ~f:(fun i form ->
           Form.view (Form.label (Ordinal_abbreviation.to_string (i + 1)) form)))
  in
  let set = function
    | Sexp.List values ->
      (match List.zip values all_forms with
       | Ok zipped ->
         Effect.Many (List.map zipped ~f:(fun (value, form) -> Form.set form value))
       | Unequal_lengths -> Effect.print_s [%message "BUG"])
    | _ -> Effect.print_s [%message "BUG:"]
  in
  Form.Expert.create ~value ~view ~set
;;

let rec generate_form
  ?(allow_updates_when_focused = `Always)
  ?textbox_for_string
  (grammar : Sexp_grammar.grammar)
  ~on_set_error
  ~customizations
  ~allow_duplication_of_list_items
  ~tooltip_format_of_doc_string
  (local_ graph)
  =
  let recurse grammar (local_ graph) =
    Bonsai.stitch
      (module Grammar)
      grammar
      ~f:(fun grammar (local_ graph) ->
        generate_form
          ~allow_updates_when_focused
          ?textbox_for_string
          grammar
          ~on_set_error
          ~customizations
          ~allow_duplication_of_list_items
          ~tooltip_format_of_doc_string
          graph)
      graph
  in
  let rec grammar_form grammar (local_ graph) = grammar.form graph
  and option_form grammar (local_ graph) =
    let%sub form, _ =
      Bonsai.wrap
        graph
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:(fun context result () sexp ->
          match result with
          | Inactive ->
            eprint_s
              [%message
                "An action sent to a [wrap] has been dropped because its input was not \
                 present. This happens when the [wrap] is inactive when it receives a \
                 message."
                  [%here]];
            ()
          | Active (_, inner) ->
            Bonsai.Apply_action_context.schedule_event context (Form.set inner sexp))
        ~f:(fun (_ : unit Bonsai.t) inject_outer (local_ graph) ->
          let outer, set_outer =
            Bonsai.state
              false
              ~sexp_of_model:[%sexp_of: Bool.t]
              ~equal:[%equal: Bool.t]
              graph
          in
          let path = Bonsai.path_id graph in
          let inner =
            match%sub outer with
            | false -> Bonsai.return (Form.return (Sexp.List []))
            | true -> grammar_form grammar graph
          in
          let inner_value =
            let%arr inner in
            Form.value inner
          in
          let view =
            let%arr outer and set_outer and path and inner in
            let toggle_view =
              E.Checkbox.Private.make_input
                ~key:path
                ~id:(Vdom.Attr.id path)
                ~extra_attrs:[]
                ~state:outer
                ~set_state:set_outer
                ()
            in
            match outer with
            | false -> Form.View.option ~toggle:toggle_view ~status:(Currently_none None)
            | true ->
              Form.View.option
                ~toggle:toggle_view
                ~status:(Currently_some (Form.view inner))
          in
          let value =
            match%sub outer with
            | false -> Bonsai.return (Ok (Sexp.List []))
            | true ->
              (match%arr inner_value with
               | Ok inner -> Ok (Sexp.List [ inner ])
               | Error _ as err -> err)
          in
          let set =
            let bonk = Bonsai_extra.Effects.bonk graph in
            let%arr set_outer and inject_outer and outer and bonk in
            function
            | Sexp.List [] | Atom "None" | Atom "none" -> set_outer false
            | List [ a ] | List [ Atom ("Some" | "some"); a ] ->
              (match outer with
               | true -> bonk (inject_outer a)
               | false -> Effect.Many [ set_outer true; bonk (inject_outer a) ])
            | sexp ->
              on_set_error [%message "expected option sexp, but got" (sexp : Sexp.t)]
          in
          let%arr view and value and set and inner in
          Form.Expert.create ~view ~value ~set, inner)
    in
    form
  and optional_field_form args (local_ graph) =
    let%sub form, _ =
      Bonsai.wrap
        graph
        ~sexp_of_model:[%sexp_of: Unit.t]
        ~equal:[%equal: Unit.t]
        ~default_model:()
        ~apply_action:(fun context result () inner_value ->
          match result with
          | Inactive ->
            eprint_s
              [%message
                "An action sent to a [wrap] has been dropped because its input was not \
                 present. This happens when the [wrap] is inactive when it receives a \
                 message."
                  [%here]];
            ()
          | Active (_, inner) ->
            Bonsai.Apply_action_context.schedule_event
              context
              (Form.set inner inner_value))
        ~f:(fun (_ : unit Bonsai.t) inject_outer (local_ graph) ->
          (* We can't use toggle here because we need to be able to set the value directly
             as part of [Form.set] *)
          let override, set_override =
            Bonsai.state
              false
              ~sexp_of_model:[%sexp_of: Bool.t]
              ~equal:[%equal: Bool.t]
              graph
          in
          let toggle =
            let%arr override and set_override in
            match args with
            | Empty ->
              Vdom_input_widgets.Checkbox.simple
                ~merge_behavior:Legacy_dont_merge
                ~is_checked:override
                ~label:""
                ~on_toggle:(set_override (not override))
                ()
            | Cons _ | Many _ | Fields _ ->
              let text = if override then "[override]" else "[default]" in
              let override_status =
                if override then Style.override_showing else Style.override_hidden
              in
              N.div
                ~attrs:
                  [ A.(
                      override_status
                      @ Style.override_text
                      @ on_click_js_of_ocaml (fun _ -> set_override (not override)))
                  ]
                [ N.text text ]
          in
          let inner =
            match%sub override with
            | false ->
              Bonsai.return
                (Form.return_error (Error.of_string "unreachable auto-gen code"))
            | true -> list_grammar_form args graph
          in
          let bonk = Bonsai_extra.Effects.bonk graph in
          let%arr override
          and set_override
          and toggle
          and bonk
          and inject_outer
          and inner in
          let view =
            match override with
            | false -> Form.View.option ~toggle ~status:(Currently_none None)
            | true -> Form.View.option ~toggle ~status:(Currently_some (Form.view inner))
          in
          let value =
            match override with
            | false -> Ok None
            | true ->
              (match Form.value inner with
               | Ok sexp -> Ok (Some sexp)
               | Error _ as err -> err)
          in
          let set = function
            | None -> set_override false
            | Some sexp -> Effect.Many [ set_override true; bonk (inject_outer sexp) ]
          in
          Form.Expert.create ~view ~value ~set, inner)
    in
    form
  and fields_grammar_form fields ~allow_extra_fields (local_ graph) =
    let sexp_dot_bool_fields =
      List.filter_map fields ~f:(fun { name; required; args; tags = _ } ->
        match required, args with
        | false, Empty -> Some name
        | true, _ | false, (Cons _ | Many _ | Fields _) -> None)
      |> String.Set.of_list
    in
    let field_forms =
      List.map fields ~f:(fun { name; required; args; tags } ->
        let doc = find_doc_tag tags in
        (* [form] is a [Sexp.t option Form.t] because we need the ability to include or
           omit the sexp produced by that part of the form depending on if we want to use
           the default value. [None] indicates that we should just use the default value. *)
        let form =
          match required with
          | true ->
            let form = list_grammar_form args graph in
            let%arr form in
            Form.project
              form
              ~parse_exn:(fun sexp -> Some sexp)
              ~unparse:(fun value -> Option.value_exn value)
          | false -> optional_field_form args graph
        in
        name, required, doc, form)
    in
    let all_forms =
      Bonsai.all
        (List.map field_forms ~f:(fun (name, required, doc, form) ->
           let%arr form in
           name, required, doc, form))
    in
    let%arr all_forms in
    let forms =
      String.Map.of_alist_exn
        (List.map all_forms ~f:(fun (name, required, doc, form) ->
           name, (form, required, doc)))
    in
    let view =
      List.map fields ~f:(fun { name; _ } ->
        let form, _, doc = Map.find_exn forms name in
        { Form.View.field_view =
            form |> Form.view |> maybe_set_tooltip ~tooltip_format_of_doc_string doc
        ; field_name = name
        })
      |> Form.View.record
    in
    let set sexp =
      let sexp_map =
        match sexp with
        | Sexp.Atom _ -> String.Map.empty
        | List fields ->
          List.filter_map fields ~f:(function
            | List [ Atom name; value ] -> Some (name, Sexp.List [ value ])
            | List [ Atom name ] when Set.mem sexp_dot_bool_fields name ->
              Some (name, Sexp.Atom "true")
            | ignored_sexp ->
              eprint_s
                [%message
                  "Potential BUG in sexp grammar auto-generated forms. Please report to \
                   bonsai developers. A sub-field of a sexp is getting ignored while \
                   calling Form.set"
                    [%here]
                    (ignored_sexp : Sexp.t)];
              None)
          |> String.Map.of_alist_exn
      in
      Map.merge sexp_map forms ~f:(fun ~key -> function
        | `Left args ->
          (match allow_extra_fields with
           | true -> None
           | false ->
             Some
               (on_set_error
                  [%message
                    "extra fields are not allowed, but got an extra field"
                      (key : string)
                      (args : Sexp.t)]))
        | `Right (form, false, _) -> Some (Form.set form None)
        | `Right (_, true, _) ->
          Some (on_set_error [%message "required field is not present" (key : string)])
        | `Both (args, (form, _, _)) -> Some (Form.set form (Some args)))
      |> Map.data
      (* This reversal of order is important and enables optimization! *)
      |> List.rev
      |> Effect.Many
    in
    let values =
      Map.filter_mapi forms ~f:(fun ~key ~data:(form, _, _) ->
        match Form.value form with
        | Error _ as err -> Some err
        | Ok None -> None
        | Ok (Some (Sexp.Atom _ as atom)) ->
          Some
            (Or_error.error_s
               [%message "expected list of args from subform, but got" (atom : Sexp.t)])
        | Ok (Some (Sexp.List value)) -> Some (Ok (Sexp.List (Sexp.Atom key :: value))))
      |> Map.data
      |> Or_error.all
      |> Or_error.map ~f:(fun fields -> Sexp.List fields)
    in
    Form.Expert.create ~value:values ~view ~set
  and list_form_with_duplication grammar (local_ graph) =
    let list_form = Form2.Elements.Multiple.list grammar.form graph in
    let theme = View.Theme.current graph in
    let%arr list_form and theme in
    let duplicate idx =
      match Form2.value list_form with
      | Error _ -> Effect.Ignore
      | Ok value ->
        Form2.set
          list_form
          (List.concat_mapi value ~f:(fun i value ->
             if i = idx then [ value; value ] else [ value ]))
    in
    let render_button ~theme ~enabled ~on_click text =
      let color =
        match enabled with
        | true -> Css_gen.color (`Name "blue")
        | false -> Css_gen.color (`Name "gray")
      in
      View.button
        theme
        ~attrs:[ Vdom.Attr.type_ "button"; Vdom.Attr.style color ]
        ~disabled:(not enabled)
        ~on_click
        text
    in
    let ({ items; add_element } : _ Form2.Elements.Multiple.t) = Form2.view list_form in
    let items =
      List.mapi items ~f:(fun i { form; remove } ->
        let remove_view =
          View.hbox
            ~gap:(`Em_float 0.5)
            [ View.textf "%d - " i
            ; render_button ~theme ~enabled:true ~on_click:remove "remove"
            ; render_button
                ~theme
                ~enabled:(Or_error.is_ok (Form2.value list_form))
                ~on_click:(duplicate i)
                "duplicate"
            ]
        in
        Form.View.list_item ~view:(Form.view form) ~remove_item:(Remove_view remove_view))
    in
    let append_view =
      render_button ~theme ~enabled:true ~on_click:add_element "Add new element"
    in
    let view =
      Form.View.list
        ~append_item:(Append_view append_view)
        ~legacy_button_position:`Indented
        items
    in
    let value =
      let%map.Or_error value = Form2.value list_form in
      Sexp.List value
    in
    let set = function
      | Sexp.List values -> Form2.set list_form values
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    Form.Expert.create ~value ~set ~view
  and list_form_without_duplication grammar (local_ graph) =
    let%arr list_form = E.Multiple.list grammar.form graph in
    let value =
      let%map.Or_error value = Form.value list_form in
      Sexp.List value
    in
    let set = function
      | Sexp.List values -> Form.set list_form values
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    Form.Expert.create ~value ~set ~view:(Form.view list_form)
  and annotate_with_ordinals grammar ~depth (local_ graph) =
    match grammar with
    | Empty -> Bonsai.return (Ok (Sexp.List []), (fun _ -> Effect.Ignore), [])
    | Many grammar ->
      let form =
        match allow_duplication_of_list_items with
        | true -> list_form_with_duplication grammar graph
        | false -> list_form_without_duplication grammar graph
      in
      let%arr form in
      Form.value form, Form.set form, [ Form.view form ]
    | Fields { fields; allow_extra_fields } ->
      let form = fields_grammar_form fields ~allow_extra_fields graph in
      let%arr form in
      Form.value form, Form.set form, [ Form.view form ]
    | Cons (grammar, rest) ->
      let list_grammar = Cons (grammar, rest) in
      let grammar_form = grammar_form grammar graph in
      let rest = annotate_with_ordinals rest ~depth:(depth + 1) graph in
      let grammar_form =
        error_hint
          (fun (local_ _graph) ->
            let%arr grammar_form in
            Form.label (Ordinal_abbreviation.to_string depth) grammar_form)
          graph
      in
      let%arr grammar_form and rest in
      let rest_value, rest_set, rest_views = rest in
      let value =
        let%bind.Or_error value = Form.value grammar_form
        and rest = rest_value in
        match rest with
        | Sexp.Atom _ as atom ->
          Or_error.error_s
            [%message
              "got unexpected atom for list grammar"
                ~grammar:
                  (sexp_grammar_of_folded_list list_grammar : Sexp_grammar.list_grammar)
                (atom : Sexp.t)]
        | List rest -> Ok (Sexp.List (value :: rest))
      in
      let set = function
        | Sexp.List (value :: rest) ->
          (* The order of these sets is important and enables optimization! *)
          Effect.Many [ rest_set (Sexp.List rest); Form.set grammar_form value ]
        | sexp ->
          on_set_error
            [%message
              "attempted to set atom into list grammar"
                ~grammar:
                  (sexp_grammar_of_folded_list list_grammar : Sexp_grammar.list_grammar)
                (sexp : Sexp.t)]
      in
      value, set, Form.view grammar_form :: rest_views
  and list_grammar_form grammar (local_ graph) =
    match grammar with
    | Empty ->
      Bonsai.return
        (Form.Expert.create
           ~value:(Ok (Sexp.List []))
           ~set:(fun _ -> Effect.Ignore)
           ~view:Form.View.empty)
    | Cons (first, Empty) -> constant_length_tuple_form ~grammar_form [ first ] graph
    | Cons (first, Cons (second, Empty)) ->
      constant_length_tuple_form ~grammar_form [ first; second ] graph
    | Cons (first, Cons (second, Cons (third, Empty))) ->
      constant_length_tuple_form ~grammar_form [ first; second; third ] graph
    | Cons (first, Cons (second, Cons (third, Cons (fourth, Empty)))) ->
      constant_length_tuple_form ~grammar_form [ first; second; third; fourth ] graph
    | Cons (first, Cons (second, Cons (third, Cons (fourth, Cons (fifth, Empty))))) ->
      constant_length_tuple_form
        ~grammar_form
        [ first; second; third; fourth; fifth ]
        graph
    | Cons
        ( first
        , Cons (second, Cons (third, Cons (fourth, Cons (fifth, Cons (sixth, Empty))))) )
      ->
      constant_length_tuple_form
        ~grammar_form
        [ first; second; third; fourth; fifth; sixth ]
        graph
    | Cons
        ( first
        , Cons
            ( second
            , Cons
                (third, Cons (fourth, Cons (fifth, Cons (sixth, Cons (seventh, Empty)))))
            ) ) ->
      constant_length_tuple_form
        ~grammar_form
        [ first; second; third; fourth; fifth; sixth; seventh ]
        graph
    | Many grammar ->
      (match allow_duplication_of_list_items with
       | true -> list_form_with_duplication grammar graph
       | false -> list_form_without_duplication grammar graph)
    | Fields { fields; allow_extra_fields } ->
      fields_grammar_form fields ~allow_extra_fields graph
    | Cons _ ->
      (* Tuples don't have labels, so we annotate their arguments with ordinals. The
         special-case check for a singleton list is because we don't want to add an
         ordinal to variants/fields that take a single argument. *)
      let%arr value, set, views = annotate_with_ordinals grammar ~depth:1 graph in
      Form.Expert.create ~value ~set ~view:(combine_views views)
  and constant_clauses_form clauses (local_ graph) =
    let names = List.map clauses ~f:(fun { name; _ } -> name) in
    let form =
      Form.Elements.Dropdown.list
        (module String)
        ~to_string:Fn.id
        ~equal:String.equal
        (Bonsai.return names)
        graph
    in
    let%arr form in
    Form.project
      form
      ~parse_exn:(fun name -> Sexp.Atom name)
      ~unparse:(function
        | Sexp.Atom name -> name
        | List _ ->
          raise_s
            [%message "BUG: invalid non-atom constructor set into constant clause form"])
  and clauses_form { case_sensitivity = _; clauses } (local_ graph) =
    let has_only_untagged_atom_clauses =
      List.for_all clauses ~f:(fun { args; tags; _ } ->
        Option.is_none args && List.is_empty tags)
    in
    match clauses, has_only_untagged_atom_clauses with
    | [], _ ->
      (* There's no value that a form can produce for a variant type with no clauses. So,
         we just produce a form that errors. *)
      Bonsai.return
        (Form.return_error (Error.create_s [%message "no clauses in variant"]))
    | _ :: _, true -> constant_clauses_form clauses graph
    | _ :: _, false ->
      let%sub form, _ =
        Bonsai.wrap
          graph
          ~sexp_of_model:[%sexp_of: Unit.t]
          ~equal:[%equal: Unit.t]
          ~default_model:()
          ~apply_action:(fun context result () sexp ->
            match result with
            | Inactive ->
              eprint_s
                [%message
                  "An action sent to a [wrap] has been dropped because its input was not \
                   present. This happens when the [wrap] is inactive when it receives a \
                   message."
                    [%here]];
              ()
            | Active (_, inner) ->
              Bonsai.Apply_action_context.schedule_event context (Form.set inner sexp))
          ~f:(fun (_ : unit Bonsai.t) inject_outer (local_ graph) ->
            let clause_names = List.map clauses ~f:(fun { name; _ } -> name) in
            let clauses_as_map =
              String.Map.of_alist_exn
                (List.map clauses ~f:(fun clause -> clause.name, clause))
            in
            let%sub outer, set_outer, outer_view =
              let open E.Dropdown.Private in
              let module Opt = struct
                type t = string Opt.t [@@deriving sexp, equal]

                let to_option = Opt.to_option
              end
              in
              let outer_and_set_outer =
                Tuple2.uncurry Bonsai.both
                @@ Bonsai.state
                     Uninitialized
                     ~sexp_of_model:[%sexp_of: Opt.t]
                     ~equal:[%equal: Opt.t]
                     graph
              in
              let path = Bonsai.path_id graph in
              let theme = View.Theme.current graph in
              let%arr path and outer_and_set_outer and theme in
              let outer, set_outer = outer_and_set_outer in
              let dropdown =
                make_input
                  ~to_string:(Form.View.sexp_to_pretty_string [%sexp_of: string])
                  (module String)
                  ~equal:[%equal: String.t]
                  ~id:(Vdom.Attr.id path)
                  ~include_empty:true
                  ~default_value:Not_provided
                  ~value_not_in_options_behavior:`Allow
                  ~state:outer
                  ~set_state:set_outer
                  ~all:clause_names
                  ~extra_attrs:
                    [ Vdom.Attr.style (Css_gen.width (`Percent (Percent.of_mult 1.))) ]
                  ~extra_option_attrs:(Fn.const [])
              in
              let info =
                List.filter_map clauses ~f:(fun { name; tags; _ } ->
                  let doc = find_doc_tag tags in
                  Option.map doc ~f:(fun doc ->
                    View.vbox
                      [ Vdom.Node.span ~attrs:[ Style.bold_text ] [ Vdom.Node.text name ]
                      ; Vdom.Node.text doc
                      ]))
              in
              let view =
                match info with
                | [] -> dropdown
                | info ->
                  View.hbox
                    [ dropdown
                    ; Vdom.Node.div
                        ~attrs:
                          [ View.tooltip_attr'
                              theme
                              ~tooltip_attrs:[ Style.scrollable_tooltip ]
                              ~hoverable_inside:true
                              ~position:Right
                              info
                          ]
                        [ Vdom.Node.text "?" ]
                    ]
              in
              ( Opt.to_option outer ~allow_illegal_values:true
              , (function
                  | None -> set_outer Explicitly_none
                  | Some outer -> set_outer (Set outer))
              , view )
            in
            let selected_clause =
              let%arr outer in
              Option.bind outer ~f:(Map.find clauses_as_map)
            in
            let module Clause_index = struct
              type t = int option [@@deriving sexp, equal, compare]

              let all = None :: List.mapi clauses ~f:(fun i _ -> Some i)
            end
            in
            let selected_index =
              let%arr selected_clause in
              Option.bind selected_clause ~f:(fun selected_clause ->
                List.find_mapi clauses ~f:(fun i clause ->
                  Option.some_if (String.equal clause.name selected_clause.name) i))
            in
            let inner =
              Bonsai.enum
                (module Clause_index)
                ~match_:selected_index
                ~with_:(function
                  | None -> fun _graph -> Bonsai.return (Form.return (Sexp.List []))
                  | Some index ->
                    (match (List.nth_exn clauses index).args with
                     | None -> fun _graph -> Bonsai.return (Form.return (Sexp.List []))
                     | Some args -> fun graph -> list_grammar_form args graph))
                graph
            in
            let bonk = Bonsai_extra.Effects.bonk graph in
            let%arr outer
            and inner
            and bonk
            and set_outer
            and inject_outer
            and outer_view in
            let view =
              match outer with
              | None ->
                Form.View.variant ~clause_selector:outer_view ~selected_clause:None
              | Some clause_name ->
                Form.View.variant
                  ~clause_selector:outer_view
                  ~selected_clause:(Some { clause_name; clause_view = Form.view inner })
            in
            let set = function
              | Sexp.List (Sexp.Atom clause_name :: args) ->
                (match Map.find clauses_as_map clause_name with
                 | Some clause ->
                   (match outer with
                    | Some name when String.equal name clause.name ->
                      bonk (inject_outer (Sexp.List args))
                    | Some _ | None ->
                      Effect.Many
                        [ set_outer (Some clause.name)
                        ; bonk (inject_outer (Sexp.List args))
                        ])
                 | None ->
                   on_set_error
                     [%message
                       "unknown clause_name while setting a clause form"
                         (clause_name : string)])
              | Sexp.Atom clause_name ->
                (match Map.find clauses_as_map clause_name with
                 | Some clause -> set_outer (Some clause.name)
                 | None ->
                   on_set_error
                     [%message
                       "unknown clause_name while setting a clause form"
                         (clause_name : string)])
              | sexp ->
                on_set_error
                  [%message
                    "unexpected format while setting a clause form" (sexp : Sexp.t)]
            in
            let value =
              match outer, Form.value inner with
              | Some clause_name, Ok (Sexp.List []) -> Ok (Sexp.Atom clause_name)
              | Some clause_name, Ok (Sexp.List args) ->
                Ok (Sexp.List (Sexp.Atom clause_name :: args))
              | Some _, Ok sexp ->
                Or_error.error_s [%message "invalid sexp encountered" (sexp : Sexp.t)]
              | None, _ -> Or_error.error_s [%message "a value is required"]
              | _, (Error _ as err) -> err
            in
            Form.Expert.create ~view ~value ~set, inner)
      in
      form
  in
  let module Callbacks = struct
    type t = folded_grammar
    type list_t = folded_list_grammar

    let create ?variant ?(find_descendant = fun _ -> None) grammar form =
      let find_form query =
        if phys_equal query grammar then Some form else find_descendant query
      in
      { grammar; form; find_form; variant }
    ;;

    let any name =
      create
        (Sexp_grammar.Any name)
        (E.Textarea.sexpable ~allow_updates_when_focused (module Sexp))
    ;;

    let bool =
      create
        Sexp_grammar.Bool
        (error_hint (E.Checkbox.bool ~default:false () |> project_to_sexp (module Bool)))
    ;;

    let char =
      create
        Sexp_grammar.Char
        (error_hint
           (E.Textbox.stringable ~allow_updates_when_focused (module Char)
            |> project_to_sexp (module Char)))
    ;;

    let integer =
      create
        Sexp_grammar.Integer
        (error_hint
           (E.Number.int ~default:0 ~step:1 ~allow_updates_when_focused ()
            |> project_to_sexp (module Int)))
    ;;

    let float =
      create
        Sexp_grammar.Float
        (error_hint
           (E.Number.float ~default:0. ~step:1. ~allow_updates_when_focused ()
            |> project_to_sexp (module Float)))
    ;;

    let string =
      let form =
        match textbox_for_string with
        | None ->
          E.Textarea.string ~allow_updates_when_focused ()
          |> project_to_sexp (module String)
        | Some () ->
          E.Textbox.string ~allow_updates_when_focused ()
          |> project_to_sexp (module String)
      in
      create Sexp_grammar.String (error_hint form)
    ;;

    let option grammar =
      create
        ~find_descendant:grammar.find_form
        (Sexp_grammar.Option grammar.grammar)
        (option_form grammar)
    ;;

    let union grammars =
      let grammar =
        Sexp_grammar.Union (List.map grammars ~f:(fun grammar -> grammar.grammar))
      in
      let find_descendant query =
        List.find_map grammars ~f:(fun grammar -> grammar.find_form query)
      in
      match grammars with
      | [] ->
        create ~find_descendant grammar (fun _graph ->
          Bonsai.return
            (Form.return_error (Error.create_s [%message "no grammars in union"])))
      (* This is a special form of union that's pretty easy to construct and used widely
         in [Css_gen], which are often inputs into Bonsai components. Special casing this
         case to have better support for those, but the general case below should still
         probably be thought about. *)
      | [ { variant = Some a; _ }; { variant = Some b; _ } ]
        when let names =
               List.map (a.clauses @ b.clauses) ~f:(fun clause -> clause.name)
             in
             Set.length (String.Set.of_list names) = List.length names ->
        let case_sensitivity =
          match a.case_sensitivity, b.case_sensitivity with
          | Case_sensitive, _ | _, Case_sensitive -> Sexp_grammar.Case_sensitive
          | Case_sensitive_except_first_character, _
          | _, Case_sensitive_except_first_character ->
            Case_sensitive_except_first_character
          | Case_insensitive, Case_insensitive -> Case_insensitive
        in
        let clauses =
          List.merge a.clauses b.clauses ~compare:(fun a b ->
            String.compare a.name b.name)
        in
        let variant = { case_sensitivity; clauses } in
        create ~variant ~find_descendant grammar (error_hint (clauses_form variant))
      | _ ->
        create
          ~find_descendant
          grammar
          (E.Textarea.sexpable ~allow_updates_when_focused (module Sexp))
    ;;

    let list grammar =
      create
        ~find_descendant:(find_form_in_list_grammar grammar)
        (Sexp_grammar.List (sexp_grammar_of_folded_list grammar))
        (list_grammar_form grammar)
    ;;

    let empty = Empty
    let cons grammar rest = Cons (grammar, rest)
    let many grammar = Many grammar

    let record fields ~allow_extra_fields =
      let fields =
        List.map fields ~f:(fun (name, (field, tags)) ->
          let required, args =
            match (field : _ Sexp_grammar.Field.t) with
            | Optional args -> false, args
            | Required args -> true, args
          in
          { name; required; args; tags })
      in
      Fields { fields; allow_extra_fields }
    ;;

    let variant clauses ~case_sensitivity =
      let clauses =
        List.map clauses ~f:(fun (name, (args, tags)) -> { name; args; tags })
      in
      let variant = { case_sensitivity; clauses } in
      let find_descendant query =
        List.find_map clauses ~f:(fun { args; _ } ->
          Option.bind args ~f:(fun args -> find_form_in_list_grammar args query))
      in
      create
        ~variant
        ~find_descendant
        (sexp_grammar_of_folded_variant variant)
        (error_hint (clauses_form variant))
    ;;

    (* Forcing [grammar] here (or reconstructing the real [Sexp_grammar.grammar] it
       describes) isn't allowed: [Fold_recursive] requires that lazies are not forced
       during the fold, so we use a placeholder grammar and delay building the form. *)
    let lazy_ grammar =
      create (Sexp_grammar.Any "lazy grammar") (fun graph ->
        Bonsai.delay ~f:(fun graph -> grammar_form (Lazy.force grammar) graph) graph)
    ;;

    let tag grammar key value =
      let tagged = { Sexp_grammar.key; value; grammar = grammar.grammar } in
      let form graph =
        match
          List.find customizations ~f:(fun customization ->
            Customization.applies customization tagged)
        with
        | None -> error_hint grammar.form graph
        | Some customization ->
          let recurse_within_customization child (local_ graph) =
            Bonsai.stitch
              (module Grammar)
              child
              ~f:(fun child (local_ graph) ->
                match grammar.find_form child with
                | Some form -> form graph
                | None -> recurse (Bonsai.return child) graph)
              graph
          in
          Customization.apply
            customization
            (Bonsai.return tagged)
            ~recurse:recurse_within_customization
            graph
      in
      create ~find_descendant:grammar.find_form (Sexp_grammar.Tagged tagged) form
    ;;

    (* [Bonsai.delay] is what ties the knot for recursive grammars: eagerly building the
       form here would recurse forever. *)
    let of_lazy_recursive grammar =
      create (Sexp_grammar.Any "recursive grammar") (fun graph ->
        Bonsai.delay ~f:(fun graph -> grammar_form (Lazy.force grammar) graph) graph)
    ;;
  end
  in
  let module Fold = Sexp_grammar.Fold_recursive (Callbacks) in
  let folded = Fold.of_grammar_exn grammar in
  grammar_form folded graph
;;

let form'
  ?(tooltip_format_of_doc_string = Vdom.Node.text)
  ?(on_set_error = Effect.print_s)
  ?allow_updates_when_focused
  ?(customizations = Customization.Defaults.Form.all ?allow_updates_when_focused ())
  ?textbox_for_string
  ?(allow_duplication_of_list_items = true)
  sexp_grammar
  (local_ graph)
  =
  Bonsai.stitch
    (module Grammar)
    sexp_grammar
    ~f:(fun sexp_grammar (local_ graph) ->
      let form =
        generate_form
          ?allow_updates_when_focused
          ?textbox_for_string
          sexp_grammar
          ~on_set_error
          ~customizations
          ~allow_duplication_of_list_items
          ~tooltip_format_of_doc_string
          graph
      in
      let validate_sexp = Sexp_grammar.validate_sexp_untyped sexp_grammar in
      let%arr form in
      Form.Expert.create ~view:(Form.view form) ~value:(Form.value form) ~set:(fun sexp ->
        match unstage validate_sexp sexp with
        | Ok () -> Form.set form sexp
        | Error error ->
          on_set_error
            [%message
              "BUG: Sexp representation of set form value does not match sexp grammar. \
               Does your sexp_of_t function match your sexp grammar?"
                ~value:(sexp : Sexp.t)
                (error : Error.t)]))
    graph
;;

let form
  (type a)
  (module M : S with type t = a)
  ?tooltip_format_of_doc_string
  ?allow_updates_when_focused
  ?on_set_error
  ?customizations
  ?textbox_for_string
  ?allow_duplication_of_list_items
  : Bonsai.graph @ local -> a Form.t Bonsai.t
  =
  fun (local_ graph) ->
  let%arr form =
    form'
      ?tooltip_format_of_doc_string
      ?allow_updates_when_focused
      ?on_set_error
      ?customizations
      ?textbox_for_string
      ?allow_duplication_of_list_items
      (Bonsai.return M.t_sexp_grammar.untyped)
      graph
  in
  Form.project form ~parse_exn:M.t_of_sexp ~unparse:M.sexp_of_t
;;

let view_as_vdom ?on_submit ?editable form =
  let on_submit =
    Option.map
      on_submit
      ~f:
        (fun
          { Form.Submit.f; handle_enter; button_text; button_attr; button_location } ->
        let on_submit = Form.value form |> Result.ok |> Option.map ~f in
        { Form.View.on_submit; handle_enter; button_text; button_attr; button_location })
  in
  Render_form.to_vdom ?on_submit ?editable (Form.view form)
;;
