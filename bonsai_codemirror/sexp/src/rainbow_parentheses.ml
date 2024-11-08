open! Core
open! Codemirror

module Colors = struct
  (* An array of classes *)
  type t = string Iarray.t

  let create ~classes : t = Nonempty_list.to_list classes |> Iarray.of_list

  let default =
    (* Colors were picked to look good on light and dark backgrounds as well as being
       color-blind friendly. *)
    lazy
      (let module Sheet =
         [%css
         stylesheet
           {|
             .rainbow-colors-0 {
               color: #e69f00;
             }
             .rainbow-colors-1 {
               color: #56b4e9;
             }
             .rainbow-colors-2 {
               color: #009e73;
             }
             .rainbow-colors-3 {
               color: #d0c100;
             }
             .rainbow-colors-4 {
               color: #0072b2;
             }
             .rainbow-colors-5 {
               color: #d55e00;
             }
             .rainbow-colors-6 {
               color: #cc79a7;
             }
             |}]
       in
      create
        ~classes:
          Sheet.For_referencing.
            [ rainbow_colors_0
            ; rainbow_colors_1
            ; rainbow_colors_2
            ; rainbow_colors_3
            ; rainbow_colors_4
            ; rainbow_colors_5
            ; rainbow_colors_6
            ])
  ;;
end

let highlight colors view decorations_ref =
  let editor_state = View.Editor_view.state view in
  let doc = State.Editor_state.doc editor_state in
  let starting_at_line =
    View.Editor_view.viewport view
    |> View.Editor_view.Viewport.from
    |> Shared.line_index_of_pos doc
  in
  let until_line =
    View.Editor_view.viewport view
    |> View.Editor_view.Viewport.to_
    |> Shared.line_index_of_pos doc
  in
  match
    Shared.Parse_driver.adhoc_tokenization editor_state ~starting_at_line ~until_line
  with
  | None -> `Waiting_for_parser
  | Some tokens ->
    let decorations =
      List.filter_map tokens ~f:(fun token ->
        match token.token_type with
        | Lparen | Rparen ->
          let nesting = token.nesting_level % Iarray.length colors in
          Some
            View.Decoration.(
              range
                ~from:token.pos.offset_start
                ~to_:token.pos.offset_end
                (mark (Mark_spec.create ~class_:(Iarray.get colors nesting) ())))
        | _ -> None)
      |> View.Decoration.set ~sort:false
    in
    decorations_ref := decorations;
    `Highlighted
;;

let extension
  ?(colors = Lazy.force Colors.default)
  ?(grammar : 'x Sexp_grammar.t option)
  ()
  =
  let field =
    match grammar with
    | Some grammar -> Shared.Parse_driver.grammared grammar.untyped
    | None -> Shared.Parse_driver.ungrammared
  in
  State.Extension.of_list
    [ field
    ; View.View_plugin.extension
        (View.View_plugin.define
           ~create:(fun view ->
             let decorations_ref = ref View.Decoration.none in
             (* If the extension is reconfigured but the doc doesn't change, we should
                attempt to re-highlight. *)
             let highlight_on_start = highlight colors view decorations_ref in
             ignore (highlight_on_start : [ `Waiting_for_parser | `Highlighted ]);
             let needs_to_redecorate = ref true in
             { update =
                 Some
                   (fun update ->
                     let doc_changed = View.View_update.doc_changed update in
                     let viewport_changed = View.View_update.viewport_changed update in
                     if doc_changed || viewport_changed then needs_to_redecorate := true;
                     match !needs_to_redecorate with
                     | false -> ()
                     | true ->
                       (match highlight colors view decorations_ref with
                        | `Waiting_for_parser -> needs_to_redecorate := true
                        | `Highlighted -> needs_to_redecorate := false))
             ; custom_state = decorations_ref
             })
           ~spec:
             (View.Plugin_spec.create
                ~decorations:(Some (fun plugin_value -> !(plugin_value.custom_state))))
           ())
    ]
;;
