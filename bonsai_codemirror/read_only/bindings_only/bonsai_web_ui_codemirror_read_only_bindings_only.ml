open! Core
open! Js_of_ocaml
module Codemirror = Codemirror_bindings

module Theme = struct
  type t = Bonsai_web_ui_view.Expert.For_codemirror.Theme.t =
    | Basic_dark
    | Basic_light
    | Gruvbox_dark
    | Nord
    | Solarized_dark
    | Solarized_light
    | Material_dark
    | Vscode_dark
    | Vscode_light
    | Vscode_default
end

let empty_extension = Codemirror.State.Extension.of_list []

(** This extension forces the entire document to be displayed in Codemirror.

    By default, Codemirror only renders the visible lines, which improves performance but
    prevents the use of browser search on the entire document. But, when you print the
    page, it will render the entire document.

    https://github.com/codemirror/view/blob/main/src/viewstate.ts#L143

    To make Ctrl+F work, we manually set a "printing" parameter to true.

    Downsides of this extension:
    - This is not an officially supported feature, so it may break in future updates.
      Despite this risk, it is a useful workaround for enabling browser search.

    - This extension can slow down Codemirror, so it should only be used for read-only
      editors. *)
let print_full_document_extension () =
  let printing_parameter = Js_of_ocaml.Js.string "printing" in
  let set_printing_value editor_view new_value =
    let view_state =
      Js_of_ocaml.Js.Unsafe.get editor_view (Js_of_ocaml.Js.string "viewState")
    in
    let current_value = Js_of_ocaml.Js.Unsafe.get view_state printing_parameter in
    match Bool.equal (Js_of_ocaml.Js.to_bool current_value) new_value with
    | true -> ()
    | false ->
      Js_of_ocaml.Js.Unsafe.set
        view_state
        printing_parameter
        (Js_of_ocaml.Js.bool new_value);
      (* The `requestMeasure` function schedules a layout measurement event for the
         current view. See:
         https://github.com/codemirror/view/blob/main/src/domobserver.ts#L152

         Codemirror automatically calls this function whenever the view might change, such
         as when scrolling or resizing the viewport. Since printing works by modifying the
         viewport size, we need to manually call `requestMeasure` whenever the printing
         state changes to ensure Codemirror updates the rendered content.

         Note: `requestMeasure` is optimized to only perform the measurement once, even if
         called multiple times. *)
      Js_of_ocaml.Js.Unsafe.meth_call editor_view "requestMeasure" [||]
  in
  Codemirror.View.View_plugin.define
    ~create:(fun editor_view ->
      set_printing_value editor_view true;
      { Codemirror.View.Plugin_value.update = None
      ; destroy = Some (fun () -> set_printing_value editor_view false)
      ; custom_state = (fun () -> ())
      })
    ()
  |> Codemirror.View.View_plugin.extension
;;

module Language = struct
  type t =
    | Plaintext
    | OCaml
    | Diff
    | Html
    | Css
    | Python
    | Common_lisp
    | Sexp
    | Scheme
    | Sql
    | Javascript
    | Markdown
    | Php
    | Rust
    | Xml
    | FSharp
  [@@deriving equal, string]

  let of_stream_parser lang =
    lang
    |> Codemirror.Stream_parser.Stream_language.define
    |> Codemirror.Stream_parser.Stream_language.to_language
    |> Codemirror.Language.extension
  ;;

  let ocaml = lazy (of_stream_parser Codemirror_ocaml.ocaml_stream_parser)
  let diff = lazy (of_stream_parser Codemirror.Diff.diff)
  let scheme = lazy (of_stream_parser Codemirror.Scheme.scheme)
  let common_lisp = lazy (of_stream_parser Codemirror.Commonlisp.common_lisp)
  let html = lazy (Codemirror.Language.extension (Codemirror.Lang_html.html ()))
  let css = lazy (Codemirror.Language.extension (Codemirror.Lang_css.css ()))
  let python = lazy (Codemirror.Language.extension (Codemirror.Lang_python.python ()))
  let sql = lazy (Codemirror.Language.extension (Codemirror.Lang_sql.sql ()))

  let javascript =
    lazy (Codemirror.Language.extension (Codemirror.Lang_javascript.javascript ()))
  ;;

  let markdown =
    lazy (Codemirror.Language.extension (Codemirror.Lang_markdown.markdown ()))
  ;;

  let php = lazy (Codemirror.Language.extension (Codemirror.Lang_php.php ()))
  let rust = lazy (Codemirror.Language.extension (Codemirror.Lang_rust.rust ()))
  let xml = lazy (Codemirror.Language.extension (Codemirror.Lang_xml.xml ()))
  let fsharp = lazy (of_stream_parser Codemirror.Mllike.fsharp)

  let to_extension = function
    | Plaintext -> empty_extension
    | OCaml -> Lazy.force ocaml
    | Diff -> Lazy.force diff
    | Html -> Lazy.force html
    | Css -> Lazy.force css
    | Python -> Lazy.force python
    | Common_lisp -> Lazy.force common_lisp
    | Sexp ->
      (* We don't have a separate highlighting for sexp yet *)
      Lazy.force common_lisp
    | Scheme -> Lazy.force scheme
    | Sql -> Lazy.force sql
    | Javascript -> Lazy.force javascript
    | Markdown -> Lazy.force markdown
    | Php -> Lazy.force php
    | Rust -> Lazy.force rust
    | Xml -> Lazy.force xml
    | FSharp -> Lazy.force fsharp
  ;;
end

module Text = struct
  let state_text state =
    state
    |> Codemirror.State.Editor_state.doc
    |> Codemirror.Text.Text.to_json
    |> String.concat ~sep:"\n"
  ;;

  let set_lines new_lines state =
    let new_lines =
      match String.split_lines new_lines with
      | [] -> [ "" ]
      | other -> other
    in
    let old_text = state_text state in
    Codemirror.State.Change_spec.single
      ~from:0
      ~to_:(Js.string old_text)##.length
      ~insert:(Codemirror.Text.Text.of_ new_lines)
      ()
  ;;

  let set_lines' new_lines state =
    let updates =
      let changes = set_lines new_lines state in
      [ Codemirror.State.Transaction_spec.create ~changes () ]
    in
    Codemirror.State.Editor_state.update state updates
  ;;
end

module Line_numbers = struct
  let line_numbers_css ~on_line_number_click =
    let open Gen_js_api in
    match on_line_number_click with
    | Some _ ->
      let spec =
        Ojs.obj [| ".cm-gutter", Ojs.obj [| "cursor", Ojs.string_to_js "pointer" |] |]
      in
      Codemirror.View.Editor_view.theme ~spec ()
    | None -> empty_extension
  ;;

  let to_extension ~line_numbers ~on_line_number_click =
    match line_numbers with
    | true ->
      let line_numbers =
        let open Codemirror.Gutter in
        let dom_event_handlers =
          let%map.Option on_click = on_line_number_click in
          let mousedown view block =
            let doc =
              Codemirror.State.Editor_state.doc (Codemirror.View.Editor_view.state view)
            in
            let location = Codemirror.Gutter.Block_info.from block in
            let line =
              Codemirror.Text.Line.number (Codemirror.Text.Text.line_at doc location)
            in
            Ui_effect.Expert.handle (on_click line) ~on_exn:(fun exn ->
              Exn.reraise exn "Unhandled exception raised in effect")
          in
          Line_numbers_config.Dom_event_handlers.create ~mousedown ()
        in
        line_numbers (Line_numbers_config.create ?dom_event_handlers ())
      in
      Codemirror.State.Extension.of_list
        [ line_numbers; line_numbers_css ~on_line_number_click ]
    | false -> empty_extension
  ;;
end

module Input = struct
  type t =
    { code : string
    ; theme : Codemirror_themes.t
    ; language : Language.t
    ; line_numbers : bool
    ; line_wrapping : bool
    ; print_full_document : bool
    ; on_line_number_click : (int -> unit Ui_effect.t) option
    ; scroll_to : int option
    ; extension : Codemirror.State.Extension.t
    }

  let sexp_of_t = sexp_of_opaque
  let read_only = lazy (Codemirror.State.Compartment.create ())
  let theme = lazy (Codemirror.State.Compartment.create ())
  let language = lazy (Codemirror.State.Compartment.create ())
  let line_numbers = lazy (Codemirror.State.Compartment.create ())
  let line_wrapping = lazy (Codemirror.State.Compartment.create ())
  let print_full_document = lazy (Codemirror.State.Compartment.create ())
  let extension = lazy (Codemirror.State.Compartment.create ())

  let configure compartment extension =
    Codemirror.State.Compartment.of_ (force compartment) extension
  ;;

  let reconfigure compartment extension =
    Codemirror.State.Compartment.reconfigure (force compartment) extension
  ;;

  let with_conversion_of_bool b =
    Codemirror.With_conversion.create ~t_to_js:Gen_js_api.Ojs.bool_to_js b
  ;;

  let to_codemirror_state input =
    let extensions =
      let highlighting_ext =
        let open Codemirror.Highlight in
        syntax_highlighting
          ~options:(Syntax_highlighting_options.create ~fallback:true ())
          default_highlight_style
          ()
      in
      let editable_ext =
        Codemirror.State.Facet.of_
          Codemirror.View.Editor_view.editable
          (with_conversion_of_bool false)
      in
      let read_only_ext =
        Codemirror.State.Facet.of_
          Codemirror.State.Editor_state.read_only
          (with_conversion_of_bool true)
      in
      [ configure read_only read_only_ext
      ; (* Custom extensions are first so they take precedence. *)
        configure extension input.extension
      ; configure
          line_numbers
          (Line_numbers.to_extension
             ~line_numbers:input.line_numbers
             ~on_line_number_click:input.on_line_number_click)
      ; highlighting_ext
      ; editable_ext
      ; configure
          line_wrapping
          (if input.line_wrapping
           then Codemirror.View.Editor_view.line_wrapping
           else empty_extension)
      ; configure
          print_full_document
          (if input.print_full_document
           then print_full_document_extension ()
           else empty_extension)
      ; configure theme (Codemirror_themes.get input.theme)
      ; configure language (Language.to_extension input.language)
      ]
    in
    Codemirror.State.Editor_state_config.create ~extensions ()
    |> Codemirror.State.Editor_state.create
    |> Text.set_lines' input.code
    |> Codemirror.State.Transaction.state
  ;;

  let while_temporarily_editable ~editor_view callback =
    let reconfigure_read_only bool =
      let ext =
        Codemirror.State.Facet.of_
          Codemirror.State.Editor_state.read_only
          (with_conversion_of_bool bool)
      in
      Codemirror.State.Transaction_spec.create ~effects:[ reconfigure read_only ext ] ()
    in
    Codemirror.State.Editor_state.update
      (Codemirror.View.Editor_view.state editor_view)
      [ reconfigure_read_only false ]
    |> Codemirror.View.Editor_view.dispatch editor_view;
    callback ~editor_view;
    Codemirror.State.Editor_state.update
      (Codemirror.View.Editor_view.state editor_view)
      [ reconfigure_read_only true ]
    |> Codemirror.View.Editor_view.dispatch editor_view
  ;;

  let transaction_to_update_editor ~editor_view ~effects ~changes =
    let transaction =
      let editor_state = Codemirror.View.Editor_view.state editor_view in
      let transaction_specs =
        Codemirror.State.Transaction_spec.create ?changes ~effects ()
      in
      Codemirror.State.Editor_state.update editor_state [ transaction_specs ]
    in
    Codemirror.View.Editor_view.dispatch editor_view transaction
  ;;

  let update ~editor_view before after =
    match phys_equal before after with
    | true -> (* Nothing to do *) ()
    | false ->
      let { theme = theme_before
          ; language = language_before
          ; line_numbers = line_numbers_before
          ; line_wrapping = line_wrapping_before
          ; on_line_number_click = on_line_number_click_before
          ; print_full_document = print_full_document_before
          ; (* [scroll_to] only takes effect on the initial render, so we ignore it when
               the view is updated. *)
            scroll_to = _
          ; extension = extension_before
          ; code = code_before
          }
        =
        before
      and { theme = theme_after
          ; language = language_after
          ; line_numbers = line_numbers_after
          ; line_wrapping = line_wrapping_after
          ; on_line_number_click = on_line_number_click_after
          ; print_full_document = print_full_document_after
          ; scroll_to = _
          ; extension = extension_after
          ; code = code_after
          }
        =
        after
      in
      (* Update each compartment that has changed. *)
      let theme =
        if [%equal: Codemirror_themes.t] theme_before theme_after
        then None
        else Some (reconfigure theme (Codemirror_themes.get theme_after))
      in
      let language =
        if [%equal: Language.t] language_before language_after
        then None
        else Some (reconfigure language (Language.to_extension language_after))
      in
      let line_numbers =
        let has_changes =
          (not ([%equal: bool] line_numbers_before line_numbers_after))
          || not
               (Option.equal
                  phys_equal
                  on_line_number_click_before
                  on_line_number_click_after)
        in
        if has_changes
        then (
          let extension =
            Line_numbers.to_extension
              ~line_numbers:line_numbers_after
              ~on_line_number_click:on_line_number_click_after
          in
          Some (reconfigure line_numbers extension))
        else None
      in
      let line_wrapping =
        match [%equal: bool] line_wrapping_before line_wrapping_after with
        | true -> None
        | false ->
          let extension =
            if line_wrapping_after
            then Codemirror.View.Editor_view.line_wrapping
            else empty_extension
          in
          Some (reconfigure line_wrapping extension)
      in
      let print_full_document =
        match [%equal: bool] print_full_document_before print_full_document_after with
        | true -> None
        | false ->
          let extension =
            if print_full_document_after
            then print_full_document_extension ()
            else empty_extension
          in
          Some (reconfigure print_full_document extension)
      in
      let extension =
        if phys_equal extension_before extension_after
        then None
        else Some (reconfigure extension extension_after)
      in
      (* Update the editor text if it has changed. *)
      let changes =
        match [%equal: string] code_before code_after with
        | true -> None
        | false ->
          let editor_state = Codemirror.View.Editor_view.state editor_view in
          Some (Text.set_lines code_after editor_state)
      in
      (* Update the editor state (if anything has changed) *)
      let effects =
        List.filter_opt
          [ theme; language; line_numbers; line_wrapping; print_full_document; extension ]
      in
      if Option.is_some changes
      then
        while_temporarily_editable
          ~editor_view
          (transaction_to_update_editor ~effects ~changes)
      else if not (List.is_empty effects)
      then transaction_to_update_editor ~editor_view ~effects ~changes
  ;;
end

module Simple_widget = struct
  type dom = Dom_html.element

  module Input = Input

  module State = struct
    type t = Codemirror.View.Editor_view.t

    let sexp_of_t = sexp_of_opaque
  end

  let name = "codemirror-readonly"

  let create (input : Input.t) =
    let state = Input.to_codemirror_state input in
    let scroll_to =
      let%bind.Option line = input.scroll_to in
      let doc = Codemirror.State.Editor_state.doc state in
      let%map.Option line =
        if line > 0 && Codemirror.Text.Text.lines doc >= line
        then Some (Codemirror.Text.Text.line doc line)
        else None
      in
      let options =
        Codemirror.View.Editor_view.Scroll_into_view_options.create ~y:"center" ()
      in
      Codemirror.View.Editor_view.scroll_into_view
        ~pos:(Codemirror.Text.Line.from line)
        ~options
        ()
    in
    let view =
      Codemirror.View.(Editor_view.create (Config.create ~state ?scroll_to ()))
    in
    view, Codemirror.View.Editor_view.dom view
  ;;

  let update ~prev_input ~input ~state ~element:_ =
    Input.update ~editor_view:state prev_input input;
    state, Codemirror.View.Editor_view.dom state
  ;;

  let destroy ~prev_input:_ ~state ~element:_ = Codemirror.View.Editor_view.destroy state

  let to_vdom_for_testing =
    `Custom
      (fun { Input.language
           ; line_wrapping
           ; line_numbers
           ; on_line_number_click
           ; print_full_document
           ; scroll_to
           ; code
           ; theme
           ; extension = _
           } ->
        Virtual_dom.Vdom.Node.create
          name
          ~attrs:
            (Virtual_dom.Vdom.Attr.create "language" (Language.to_string language)
             :: Virtual_dom.Vdom.Attr.create
                  "theme"
                  (Bonsai_web_ui_view.Expert.For_codemirror.Theme.sexp_of_t theme
                   |> Sexp.to_string_hum)
             :: List.filter_opt
                  [ Option.some_if
                      line_wrapping
                      (Virtual_dom.Vdom.Attr.bool_property "line_wrapping" true)
                  ; Option.some_if
                      line_numbers
                      (Virtual_dom.Vdom.Attr.bool_property "line_numbers" true)
                  ; Option.some_if
                      print_full_document
                      (Virtual_dom.Vdom.Attr.bool_property "print_full_document" true)
                  ; Option.map on_line_number_click ~f:(fun _ ->
                      Virtual_dom.Vdom.Attr.create "@on_line_number_click" "")
                  ; Option.map scroll_to ~f:(fun scroll_to ->
                      Virtual_dom.Vdom.Attr.create "scroll_to" (Int.to_string scroll_to))
                  ])
          [ Virtual_dom.Vdom.Node.text code ])
  ;;
end

let create = unstage (Virtual_dom.Vdom.Node.widget_of_module (module Simple_widget))

let make
  ?(extension = empty_extension)
  ?(line_numbers = false)
  ?(line_wrapping = false)
  ?(print_full_document = false)
  ?on_line_number_click
  ?scroll_to
  ~language
  ~theme
  code
  =
  create
    { language
    ; code
    ; theme
    ; line_wrapping
    ; line_numbers
    ; print_full_document
    ; on_line_number_click
    ; scroll_to
    ; extension
    }
;;
