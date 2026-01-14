open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module N = Vdom.Node
module A = Vdom.Attr
module Form = Bonsai_web_ui_form.With_automatic_view
module Form2 = Bonsai_web_ui_form.With_manual_view
module E = Form.Elements

module type S = sig
  type t [@@deriving sexp, sexp_grammar]
end

type form_transformer =
  Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t
  -> recurse:
       (Sexp_grammar.grammar Bonsai.t -> local_ Bonsai.graph -> Sexp.t Form.t Bonsai.t)
  -> local_ Bonsai.graph
  -> Sexp.t Form.t Bonsai.t

let error_hint form (local_ graph) =
  let form = form graph in
  Form.Dynamic.error_hint form graph
;;

let no_duplicate_clause_names clauses =
  let rec loop acc = function
    | [] -> true
    | hd :: tl ->
      let { Sexp_grammar.name; _ } = Grammar_helper.Tags.strip_tags hd in
      (match Set.mem acc name with
       | false -> loop (Set.add acc name) tl
       | true -> false)
  in
  loop String.Set.empty clauses
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
            -> local_ Bonsai.graph
            -> Sexp.t Form.t Bonsai.t)
      -> local_ Bonsai.graph
      -> a Form.t Bonsai.t)
    =
    let transform grammar ~recurse (local_ graph) =
      let%map.Bonsai on_match = on_match grammar ~recurse graph in
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
            -> local_ Bonsai.graph
            -> Sexp.t Form.t Bonsai.t)
      -> local_ Bonsai.graph
      -> Sexp.t Form.t Bonsai.t)
    =
    transform_form' (module Sexp) ~apply_to_tag on_match
  ;;

  let constant_form
    (type a)
    (module M : Sexpable with type t = a)
    ~apply_to_tag
    (on_match : local_ Bonsai.graph -> a Form.t Bonsai.t)
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
            Sexp_grammar.grammar Bonsai.t -> local_ Bonsai.graph -> Sexp.t Form.t Bonsai.t)
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
  (form : local_ Bonsai.graph -> a Form.t Bonsai.t)
  (local_ graph)
  =
  let%map.Bonsai form = form graph in
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

module Environment = struct
  type t =
    { defns : Sexp_grammar.defn String.Map.t
    ; variables : (Sexp_grammar.grammar * t) String.Map.t
    }

  let empty = { defns = String.Map.empty; variables = String.Map.empty }
  let lookup_variable_and_env env key = Map.find_exn env.variables key
  let lookup_defn env key = Map.find_exn env.defns key

  let add_variables env ~tyvars ~instances =
    { env with
      variables =
        Map.merge
          env.variables
          (String.Map.of_alist_exn (List.zip_exn tyvars instances))
          ~f:(fun ~key:_ -> function
            | `Right grammar | `Both (_, grammar) -> Some (grammar, env)
            | `Left grammar_and_env -> Some grammar_and_env)
    }
  ;;

  let add_defns env defns =
    { env with
      defns =
        Map.merge
          env.defns
          (String.Map.of_alist_exn
             (List.map defns ~f:(fun (defn : Sexp_grammar.defn) -> defn.tycon, defn)))
          ~f:(fun ~key:_ -> function
            | `Right v | `Both (_, v) -> Some v
            | `Left v -> Some v)
    }
  ;;
end

let constant_length_tuple_form ~grammar_form list_of_grammars (local_ graph) =
  let all_forms =
    Bonsai.all (List.map list_of_grammars ~f:(fun grammar -> grammar_form grammar graph))
  in
  let%arr all_forms in
  let value =
    Or_error.all (List.map all_forms ~f:Form.value)
    |> Or_error.map ~f:(fun l -> Sexp.List l)
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
    | Sexp.List l ->
      (match List.zip l all_forms with
       | Ok zipped ->
         Effect.Many (List.map zipped ~f:(fun (value, form) -> Form.set form value))
       | Unequal_lengths -> Effect.print_s [%message "BUG"])
    | _ -> Effect.print_s [%message "BUG:"]
  in
  Form.Expert.create ~value ~view ~set
;;

let form
  ?(allow_updates_when_focused = `Always)
  ?textbox_for_string
  (grammar : Sexp_grammar.grammar Bonsai.t)
  ~on_set_error
  ~customizations
  ~allow_duplication_of_list_items
  ~tooltip_format_of_doc_string
  (local_ graph)
  =
  let with_tag_form
    ~grammar_form
    (with_tag : Sexp_grammar.grammar Sexp_grammar.with_tag Bonsai.t)
    (local_ graph)
    =
    let customization_to_use =
      let%arr with_tag in
      List.find_mapi customizations ~f:(fun i customization ->
        match Customization.applies customization with_tag with
        | false -> None
        | true -> Some i)
    in
    Bonsai.enum
      (module struct
        type t = int option [@@deriving sexp, equal, compare]

        let all = None :: List.init (List.length customizations) ~f:(fun i -> Some i)
      end)
      ~match_:customization_to_use
      ~with_:(function
        | None ->
          fun (local_ graph) ->
            let grammar =
              let%arr with_tag in
              with_tag.grammar
            in
            error_hint (grammar_form grammar) graph
        | Some index ->
          fun (local_ graph) ->
            let customization = List.nth_exn customizations index in
            Customization.apply customization with_tag ~recurse:grammar_form graph)
      graph
  in
  let option_form ~grammar_form (grammar : Sexp_grammar.grammar Bonsai.t) (local_ graph) =
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
          let inner_value = Bonsai.map ~f:Form.value inner in
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
               | Error err -> Error err)
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
            | _ as sexp ->
              on_set_error [%message "expected option sexp, but got" (sexp : Sexp.t)]
          in
          let%arr view and value and set and inner in
          Form.Expert.create ~view ~value ~set, inner)
    in
    form
  in
  let fields_grammar_form
    ~list_grammar_form
    ~optional_field_grammar_form
    (fields : Sexp_grammar.record Bonsai.t)
    (local_ graph)
    =
    let%sub { fields; allow_extra_fields } = fields in
    let original_field_order =
      let%arr fields in
      fields
      |> List.map ~f:Grammar_helper.Tags.strip_tags
      |> List.map ~f:(fun { name; _ } -> name)
    in
    let sexp_dot_bool_fields =
      let%arr fields in
      List.filter_map fields ~f:(fun field ->
        let field, _ = Grammar_helper.Tags.collect_and_strip_tags field in
        match field.required with
        | true -> None
        | false ->
          (match field.args with
           | Cons _ | Many _ | Fields _ -> None
           | Empty -> Some field.name))
      |> String.Set.of_list
    in
    let forms =
      let fields_by_name =
        let%arr fields in
        List.map fields ~f:(fun (field : Sexp_grammar.field Sexp_grammar.with_tag_list) ->
          let field, tags = Grammar_helper.Tags.collect_and_strip_tags field in
          let doc = Grammar_helper.Tags.find_doc_tag tags in
          field.name, (field, doc))
        |> String.Map.of_alist_exn
      in
      Bonsai.assoc
        (module String)
        fields_by_name
        ~f:
          (fun
            _
            field_and_doc
            (* [args_form] is a [Sexp.t option Form.t] because we need the ability to
               include or omit the sexp produced by that part of the form depending on if
               we want to use the default value. [None] indicates that we should just use
               the default value. *)
            (local_ graph)
          ->
          let%sub field, doc = field_and_doc in
          let required =
            let%arr field in
            field.required
          in
          let args =
            let%arr field in
            field.args
          in
          let args_form =
            match%sub required with
            | true ->
              let%map.Bonsai form = list_grammar_form args graph in
              Form.project
                form
                ~parse_exn:(fun s -> Some s)
                ~unparse:(fun s -> Option.value_exn s)
            | false -> optional_field_grammar_form args graph
          in
          let%arr args_form and required and doc in
          args_form, `Required required, `Doc doc)
        graph
    in
    let view =
      let%arr forms and original_field_order in
      List.map original_field_order ~f:(fun field_name ->
        let form, `Required _, `Doc doc = Map.find_exn forms field_name in
        { Form.View.field_view =
            form |> Form.view |> maybe_set_tooltip ~tooltip_format_of_doc_string doc
        ; field_name
        })
      |> Form.View.record
    in
    let set =
      let%arr forms and allow_extra_fields and sexp_dot_bool_fields in
      fun sexp ->
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
                    "Potential BUG in sexp grammar auto-generated forms. Please report \
                     to bonsai developers. A sub-field of a sexp is getting ignored \
                     while calling Form.set"
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
          | `Right (form, `Required false, `Doc _) -> Some (Form.set form None)
          | `Right (_, `Required true, `Doc _) ->
            Some (on_set_error [%message "required field is not present" (key : string)])
          | `Both (args, (form, `Required _, `Doc _)) -> Some (Form.set form (Some args)))
        |> Map.data
        (* This reversal of order is important and enables optimization! *)
        |> List.rev
        |> Effect.Many
    in
    let value =
      let values =
        Bonsai.Map.filter_mapi
          forms
          ~f:(fun ~key ~data:(form, `Required _, `Doc _) ->
            match Form.value form with
            | Error _ as err -> Some err
            | Ok None -> None
            | Ok (Some (Sexp.Atom _ as atom)) ->
              Some
                (Or_error.error_s
                   [%message
                     "expected list of args from subform, but got" (atom : Sexp.t)])
            | Ok (Some (Sexp.List value)) ->
              Some (Ok (Sexp.List (Sexp.Atom key :: value))))
          graph
      in
      let%arr values in
      Map.data values |> Or_error.all |> Or_error.map ~f:(fun list -> Sexp.List list)
    in
    let%arr value and view and set in
    value, set, [ view ]
  in
  let optional_field_grammar_form
    ~list_grammar_form
    (args : Sexp_grammar.list_grammar Bonsai.t)
    (local_ graph)
    =
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
        ~f:
          (fun
            (_ : unit Bonsai.t)
            inject_outer
            (* We can't use toggle here because we need to be able to set the value
               directly as part of [Form.set] *)
            (local_ graph)
          ->
          let override, set_override =
            Bonsai.state
              false
              ~sexp_of_model:[%sexp_of: Bool.t]
              ~equal:[%equal: Bool.t]
              graph
          in
          let toggle =
            let%arr override and set_override and args in
            match args with
            | Empty ->
              Vdom_input_widgets.Checkbox.simple
                ~merge_behavior:Legacy_dont_merge
                ~is_checked:override
                ~label:""
                ~on_toggle:(set_override (not override))
                ()
            | _ ->
              let text = if override then "[override]" else "[default]" in
              let override_status =
                if override then Style.override_showing else Style.override_hidden
              in
              N.div
                ~attrs:
                  [ A.(
                      override_status
                      @ Style.override_text
                      @ on_click (fun _ -> set_override (not override)))
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
               | Ok s -> Ok (Some s)
               | Error _ as err -> err)
          in
          let set = function
            | None -> set_override false
            | Some s -> Effect.Many [ set_override true; bonk (inject_outer s) ]
          in
          Form.Expert.create ~view ~value ~set, inner)
    in
    form
  in
  let list_form_with_duplication
    ~grammar_form
    (grammar : Sexp_grammar.grammar Bonsai.t)
    (local_ graph)
    =
    let list_form =
      let form = grammar_form grammar in
      Form2.Elements.Multiple.list form graph
    in
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
    let view =
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
          Form.View.list_item
            ~view:(Form.view form)
            ~remove_item:(Remove_view remove_view))
      in
      let append_view =
        render_button ~theme ~enabled:true ~on_click:add_element "Add new element"
      in
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
      | Sexp.List l -> Form2.set list_form l
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    value, set, view
  in
  let list_form_without_duplication
    ~grammar_form
    (grammar : Sexp_grammar.grammar Bonsai.t)
    (local_ graph)
    =
    let%map.Bonsai list_form = (grammar_form grammar |> E.Multiple.list) graph in
    let view = Form.view list_form in
    let value =
      let%map.Or_error value = Form.value list_form in
      Sexp.List value
    in
    let set = function
      | Sexp.List l -> Form.set list_form l
      | Sexp.Atom _ as atom ->
        on_set_error [%message "attempted to set atom into list grammar" (atom : Sexp.t)]
    in
    value, set, view
  in
  let annotate_with_ordinals
    ~grammar_form
    ~fields_grammar_form
    (grammar : Sexp_grammar.list_grammar Bonsai.t)
    :  local_ Bonsai.graph
    -> (Sexp.t Or_error.t * (Sexp.t -> unit Effect.t) * Form.View.t list) Bonsai.t
    =
    fun (local_ graph) ->
    let open struct
      type context =
        { grammar : Sexp_grammar.list_grammar
        ; depth : int
        }
    end in
    let initial_context =
      let%arr grammar in
      { grammar; depth = 1 }
    in
    Bonsai.fix
      initial_context
      ~f:(fun ~recurse context (local_ graph) ->
        let list_form_without_duplication = list_form_without_duplication ~grammar_form in
        let list_form_with_duplication = list_form_with_duplication ~grammar_form in
        match%sub context with
        | { grammar = Empty; depth = _ } ->
          Bonsai.return (Ok (Sexp.List []), (fun _ -> Effect.Ignore), [])
        | { grammar = Many grammar; depth = _ } ->
          let value_set_and_view =
            match allow_duplication_of_list_items with
            | false -> list_form_without_duplication grammar graph
            | true -> list_form_with_duplication grammar graph
          in
          let%arr value_set_and_view in
          let value, set, view = value_set_and_view in
          value, set, [ view ]
        | { grammar = Fields fields; depth = _ } -> fields_grammar_form fields graph
        | { grammar = Cons (g, rest); depth } ->
          let g_form = grammar_form g graph in
          let rest_value_set_and_views =
            let context =
              let%arr depth and rest in
              { grammar = rest; depth = depth + 1 }
            in
            recurse context graph
          in
          let g_form =
            error_hint
              (fun (local_ _graph) ->
                let%arr g_form and depth in
                Form.label (Ordinal_abbreviation.to_string depth) g_form)
              graph
          in
          let%arr g_form and rest_value_set_and_views and grammar in
          let rest_value, rest_set, rest_views = rest_value_set_and_views in
          let value =
            let%bind.Or_error g = Form.value g_form
            and rest = rest_value in
            match rest with
            | Sexp.Atom _ as atom ->
              Or_error.error_s
                [%message
                  "got unexpected atom for list grammar"
                    (grammar : Sexp_grammar.list_grammar)
                    (atom : Sexp.t)]
            | List l -> Ok (Sexp.List (g :: l))
          in
          let set = function
            | Sexp.List (g :: rest) ->
              (* The order of these sets is important and enables optimization! *)
              Effect.Many [ rest_set (Sexp.List rest); Form.set g_form g ]
            | sexp ->
              on_set_error
                [%message
                  "attempted to set atom into list grammar"
                    (grammar : Sexp_grammar.list_grammar)
                    (sexp : Sexp.t)]
          in
          let views = Form.view g_form :: rest_views in
          value, set, views)
      graph
  in
  let fields_grammar_form ~list_grammar_form =
    fields_grammar_form
      ~list_grammar_form
      ~optional_field_grammar_form:(optional_field_grammar_form ~list_grammar_form)
  in
  let list_grammar_form
    ~grammar_form
    (grammar : Sexp_grammar.list_grammar Bonsai.t)
    (local_ graph)
    =
    Bonsai.fix
      grammar
      ~f:(fun ~recurse grammar (local_ graph) ->
        let fields_grammar_form = fields_grammar_form ~list_grammar_form:recurse in
        let list_form_without_duplication = list_form_without_duplication ~grammar_form in
        let list_form_with_duplication = list_form_with_duplication ~grammar_form in
        match%sub grammar with
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
            , Cons (second, Cons (third, Cons (fourth, Cons (fifth, Cons (sixth, Empty)))))
            ) ->
          constant_length_tuple_form
            ~grammar_form
            [ first; second; third; fourth; fifth; sixth ]
            graph
        | Cons
            ( first
            , Cons
                ( second
                , Cons
                    ( third
                    , Cons (fourth, Cons (fifth, Cons (sixth, Cons (seventh, Empty)))) )
                ) ) ->
          constant_length_tuple_form
            ~grammar_form
            [ first; second; third; fourth; fifth; sixth; seventh ]
            graph
        | Many grammar ->
          let value_set_and_view =
            match allow_duplication_of_list_items with
            | false -> list_form_without_duplication grammar graph
            | true -> list_form_with_duplication grammar graph
          in
          let%arr value_set_and_view in
          let value, set, view = value_set_and_view in
          Form.Expert.create ~value ~set ~view
        | Fields fields ->
          let value_set_and_views = fields_grammar_form fields graph in
          let%arr value_set_and_views in
          let value, set, views = value_set_and_views in
          let view = combine_views views in
          Form.Expert.create ~value ~set ~view
        | Cons _ ->
          (* Tuples don't have labels, so we annotate their arguments with ordinals. The
             special-case check for a singleton list is because we don't want to add an
             ordinal to variants/fields that take a single argument. *)
          let%map.Bonsai value, set, views =
            annotate_with_ordinals ~grammar_form ~fields_grammar_form grammar graph
          in
          let view = combine_views views in
          Form.Expert.create ~value ~set ~view)
      graph
  in
  let constant_clauses_form
    (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list Bonsai.t)
    (local_ graph)
    =
    let clauses =
      let%arr clauses in
      List.map clauses ~f:(fun clause -> (Grammar_helper.Tags.strip_tags clause).name)
    in
    let form =
      Form.Elements.Dropdown.list
        (module String)
        ~to_string:Fn.id
        ~equal:String.equal
        clauses
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
  in
  let clauses_form
    ~grammar_form
    (clauses : (Sexp_grammar.grammar * 'a) Bonsai.t)
    (local_ graph)
    =
    let list_grammar_form = list_grammar_form ~grammar_form in
    let f (clauses : Sexp_grammar.clause Sexp_grammar.with_tag_list list) =
      let open Grammar_helper in
      List.for_all clauses ~f:(fun clause ->
        let clause, tags = Tags.collect_and_strip_tags clause in
        match Tags.is_empty tags, clause.clause_kind with
        | true, Atom_clause -> true
        | false, Atom_clause | false, List_clause _ | true, List_clause _ -> false)
    in
    match%sub clauses with
    | Variant { case_sensitivity = _; clauses }, _ when f clauses ->
      constant_clauses_form clauses graph
    | Variant _, _ ->
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
            let%sub clauses_names, clauses_as_map, clauses_and_docs =
              let%arr clauses in
              let clauses =
                match clauses with
                | Variant { clauses; _ }, _ -> clauses
                | _ -> assert false
              in
              let clauses_and_docs =
                List.map clauses ~f:(fun clause ->
                  let clause, tags = Grammar_helper.Tags.collect_and_strip_tags clause in
                  clause, Grammar_helper.Tags.find_doc_tag tags)
              in
              let as_map =
                String.Map.of_alist_exn
                  (List.map clauses_and_docs ~f:(fun ((clause, _doc) as clause_and_tag) ->
                     clause.name, clause_and_tag))
              in
              let just_the_names =
                List.map clauses_and_docs ~f:(fun (clause, _doc) -> clause.name)
              in
              just_the_names, as_map, clauses_and_docs
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
              let%arr path
              and outer_and_set_outer
              and clause_names = clauses_names
              and clauses_as_map
              and theme in
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
                Map.fold
                  clauses_as_map
                  ~init:[]
                  ~f:(fun ~key:name ~data:(_clause, doc) acc ->
                    match doc with
                    | None -> acc
                    | Some doc ->
                      View.vbox
                        [ Vdom.Node.span
                            ~attrs:[ Style.bold_text ]
                            [ Vdom.Node.text name ]
                        ; Vdom.Node.text doc
                        ]
                      :: acc)
                |> List.rev
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
            let clauses_forms =
              Bonsai.assoc
                (module String)
                clauses_as_map
                ~f:(fun name clause (local_ graph) ->
                  let%sub { Sexp_grammar.clause_kind; _ }, _doc = clause in
                  let is_active =
                    let%arr outer and name in
                    match outer with
                    | None -> false
                    | Some clause_name when String.equal clause_name name -> true
                    | Some _ -> false
                  in
                  if%sub is_active
                  then (
                    match%sub clause_kind with
                    | Atom_clause -> Bonsai.return (Form.return (Sexp.List []))
                    | List_clause { args : Sexp_grammar.list_grammar } ->
                      list_grammar_form args graph)
                  else Bonsai.return (Form.return (Sexp.List [])))
                graph
            in
            let bonk = Bonsai_extra.Effects.bonk graph in
            let%arr outer
            and clauses_forms
            and bonk
            and set_outer
            and inject_outer
            and clauses_and_docs
            and outer_view in
            let inner =
              match outer with
              | None -> Form.return (Sexp.List [])
              | Some selected_name ->
                (match Map.find clauses_forms selected_name with
                 | Some form -> form
                 | None ->
                   {| BUG: auto-generated forms could not find a form with the selected variant name |}
                   |> Error.of_string
                   |> Form.return_error)
            in
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
                (match
                   List.find clauses_and_docs ~f:(fun (clause, _docs) ->
                     String.equal clause.name clause_name)
                 with
                 | Some (clause, _docs) ->
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
                (match
                   List.find clauses_and_docs ~f:(fun (clause, _docs) ->
                     String.equal clause.name clause_name)
                 with
                 | Some (clause, _docs) -> set_outer (Some clause.name)
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
    | _ -> Bonsai.return (Form.return (Sexp.List []))
  in
  let grammar_form (grammar : Sexp_grammar.grammar Bonsai.t)
    : local_ Bonsai.graph -> Sexp.t Form.t Bonsai.t
    =
    fun (local_ graph) ->
    Bonsai.fix
      (Bonsai.both grammar (Bonsai.return Environment.empty))
      ~f:(fun ~recurse grammar_and_environment (local_ graph) ->
        let grammar_form grammar (local_ graph) =
          let grammar_and_environment =
            let%arr grammar and grammar_and_environment in
            let _, environment = grammar_and_environment in
            grammar, environment
          in
          recurse grammar_and_environment graph
        in
        let list_grammar_form = list_grammar_form ~grammar_form in
        let option_form = option_form ~grammar_form in
        let with_tag_form = with_tag_form ~grammar_form in
        let clauses_form = clauses_form ~grammar_form in
        match%sub grammar_and_environment with
        | Bool, _ ->
          error_hint
            (E.Checkbox.bool ~default:false () |> project_to_sexp (module Bool))
            graph
        | String, _ ->
          error_hint
            (match textbox_for_string with
             | None ->
               E.Textarea.string ~allow_updates_when_focused ()
               |> project_to_sexp (module String)
             | Some () ->
               E.Textbox.string ~allow_updates_when_focused ()
               |> project_to_sexp (module String))
            graph
        | Integer, _ ->
          error_hint
            (E.Number.int ~default:0 ~step:1 ~allow_updates_when_focused ()
             |> project_to_sexp (module Int))
            graph
        | Char, _ ->
          error_hint
            (E.Textbox.stringable ~allow_updates_when_focused (module Char)
             |> project_to_sexp (module Char))
            graph
        | Float, _ ->
          error_hint
            (E.Number.float ~default:0. ~step:1. ~allow_updates_when_focused ()
             |> project_to_sexp (module Float))
            graph
        | Option g, _ -> option_form g graph
        | List l, _ -> list_grammar_form l graph
        | Lazy g, _ ->
          (* Fearlessly force the lazy; it's not used for recursion *)
          let g =
            let%arr g in
            Portable_lazy.force g
          in
          grammar_form g graph
        | Tagged with_tag, _ -> with_tag_form with_tag graph
        | Variant { case_sensitivity = _; clauses = [] }, _ ->
          (* There's no value that a form can produce for a variant type with no clauses.
             So, we just produce a form that errors. *)
          Bonsai.return
            (Form.return_error (Error.create_s [%message "no clauses in variant"]))
        | Variant _, _ -> error_hint (clauses_form grammar_and_environment) graph
        | Union [], _ ->
          Bonsai.return
            (Form.return_error (Error.create_s [%message "no grammars in union"]))
        (* This is a special form of union that's pretty easy to construct and used widely
           in [Css_gen], which are often inputs into Bonsai components. Special casing
           this case to have better support for those, but the general case below should
           still probably be thought about. *)
        | ( Union
              [ Variant { case_sensitivity = sens_a; clauses = clauses_a }
              ; Variant { case_sensitivity = sens_b; clauses = clauses_b }
              ]
          , _ )
          when no_duplicate_clause_names (clauses_a @ clauses_b) ->
          let open Sexp_grammar in
          let merged_variant =
            let%arr sens_a and sens_b and clauses_a and clauses_b in
            let strictest_case_sensitivity =
              match sens_a, sens_b with
              | Case_sensitive, _ | _, Case_sensitive -> Case_sensitive
              | Case_sensitive_except_first_character, _
              | _, Case_sensitive_except_first_character ->
                Case_sensitive_except_first_character
              | Case_insensitive, Case_insensitive -> Case_insensitive
            in
            Variant
              { case_sensitivity = strictest_case_sensitivity
              ; clauses =
                  List.merge
                    clauses_a
                    clauses_b
                    ~compare:(compare_with_tag_list compare_clause)
              }
          in
          grammar_form merged_variant graph
        | (Any _ | Union _), _ ->
          E.Textarea.sexpable ~allow_updates_when_focused (module Sexp) graph
        | Tyvar variable, environment ->
          (* We need to interpret the looked up variable in its original environment, not
             the current environment *)
          let grammar_and_environment =
            let%arr variable and environment in
            Environment.lookup_variable_and_env environment variable
          in
          recurse grammar_and_environment graph
        | Tycon (_, _, _), _ ->
          let grammar_and_environment_with_defns =
            let%arr grammar_and_environment in
            let key, instances, defns, environment =
              match grammar_and_environment with
              | Tycon (key, instances, defns), environment ->
                key, instances, defns, environment
              | _ -> assert false
            in
            ( Sexp_grammar.Recursive (key, instances)
            , Environment.add_defns environment defns )
          in
          recurse grammar_and_environment_with_defns graph
        | Recursive (_, _), _ ->
          let grammar_and_environment =
            let%arr grammar_and_environment in
            let tycon, instances, environment =
              match grammar_and_environment with
              | Recursive (tycon, instances), environment -> tycon, instances, environment
              | _ -> assert false
            in
            let ({ tycon = _; tyvars; grammar } : Sexp_grammar.defn) =
              Environment.lookup_defn environment tycon
            in
            grammar, Environment.add_variables environment ~tyvars ~instances
          in
          recurse grammar_and_environment graph)
      graph
  in
  grammar_form grammar graph
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
  let form =
    form
      ?allow_updates_when_focused
      ?textbox_for_string
      sexp_grammar
      ~on_set_error
      ~customizations
      ~allow_duplication_of_list_items
      ~tooltip_format_of_doc_string
      graph
  in
  let%arr form and sexp_grammar in
  let validate_sexp = Sexp_grammar.validate_sexp_untyped sexp_grammar in
  Form.Expert.create ~view:(Form.view form) ~value:(Form.value form) ~set:(fun sexp ->
    match unstage validate_sexp sexp with
    | Ok () -> Form.set form sexp
    | Error error ->
      on_set_error
        [%message
          "BUG: Sexp representation of set form value does not match sexp grammar. Does \
           your sexp_of_t function match your sexp grammar?"
            ~value:(sexp : Sexp.t)
            (error : Error.t)])
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
  : local_ Bonsai.graph -> a Form.t Bonsai.t
  =
  fun (local_ graph) ->
  let%map.Bonsai form =
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
