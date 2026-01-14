open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Styling = Bonsai_web_ui_partial_render_table_styling

module Which_styling = struct
  type t =
    | This_one of Styling.t Bonsai.t
    | From_theme
    | Legacy_unsafe_raw_classnames
end

module Themed = struct
  type t = Styling.Expert.t

  module Prt_view = Bonsai_web_ui_view.For_components.Prt

  let resolve ~resize_column_widths_to_fit which_styling (local_ graph) =
    let theme = View.Theme.current graph in
    match which_styling with
    | Which_styling.Legacy_unsafe_raw_classnames ->
      return
        { Styling.Expert.header_cell =
            {%css|
              text-align: center;
              font-weight: bold;
            |}
        ; header_cell_focused = Vdom.Attr.empty
        ; header_row = Vdom.Attr.empty
        ; header = Vdom.Attr.class_ "prt-table-header"
        ; autosize_table_cell_wrapper = Vdom.Attr.empty
        ; autosize_table_cell_wrapper_focused = Vdom.Attr.empty
        ; cell = Vdom.Attr.class_ "prt-table-cell"
        ; cell_focused = Vdom.Attr.class_ "prt-table-cell-selected"
        ; row = Vdom.Attr.class_ "prt-table-row"
        ; row_focused = Vdom.Attr.class_ "prt-table-row-selected"
        ; row_of_focused_cell = Vdom.Attr.empty
        ; body = Vdom.Attr.empty
        ; table = Vdom.Attr.empty
        ; table_vars = Vdom.Attr.empty
        }
    | From_theme ->
      let%arr theme and resize_column_widths_to_fit in
      Styling.Private.resolve ~resize_column_widths_to_fit (Prt_view.styling theme)
    | This_one styling ->
      let%arr styling and resize_column_widths_to_fit in
      Styling.Private.resolve ~resize_column_widths_to_fit styling
  ;;
end

(* These styles make the table functional and interactive; they are applied regardless of
   theme. *)
module Functional_style =
  [%css
  stylesheet
    {|
      .partial_render_table_container * {
        box-sizing: border-box;
      }

      /* The default value for the [overflow-anchor] CSS property is [auto], which
         permits the browser to scroll the page in order to minimize content shifts.
         This interacts poorly with the PRT because our virtual-dom diff-and-patch
         algorithm often removes and re-inserts elements. To fix this, we disable
         overflow-anchor on the partial render table. This disallows using the PRT
         and its descendants in the anchor node selection algorithm */

      .partial_render_table_container {
        width: max-content;
        position: relative;
        overflow-anchor: none;
      }

      .default_partial_render_table_body {
        position: relative;
      }

      .sortable_header_cell {
        white-space: pre;
        cursor: pointer;
      }

      .header_label {
        user-select: none;
      }

      .leaf_header {
        overflow: hidden;
      }

      .leaf_header_resizable {
        resize: horizontal;
        padding-right: 10px; /* Space for the resizer */
      }

      .partial_render_table_header {
        position: sticky;
        top: 0px;
        z-index: 99;
      }

      .cell {
        overflow: hidden;
        display: inline-block;
        contain: strict;
      }

      .autosize_wrapped_cell {
        display: block;
      }

      .autosize_table_cell_wrapper {
        display: table-cell;
      }
    |}]

(* This function takes a vdom node and if it's an element, it adds extra attrs, classes,
   key, and style info to it, but if it's not an element, it wraps that node in a div that
   has those attributes. This can be useful if you get a vdom node from the user of this
   API, and want to avoid excessive node wrapping. *)
let set_or_wrap ~attrs =
  let open Vdom.Node in
  function
  | Element e -> Element (Element.map_attrs e ~f:(fun a -> Vdom.Attr.(a @ many attrs)))
  | other -> div ~attrs [ other ]
;;

let int_to_px_string px = Int.to_string px ^ "px"
let float_to_px_string px = Virtual_dom.Dom_float.to_string_fixed 2 px ^ "px"

module Header_label = struct
  let wrap_clickable ?test_selector ~sortable ~handle_click contents =
    let test_selector =
      Option.map test_selector ~f:Test_selector.attr |> Option.to_list
    in
    let attrs =
      (if sortable
       then
         [ Functional_style.sortable_header_cell
         ; handle_click
         ; Vdom.Attr.role "button"
         ; Vdom.Attr.tabindex 0
         ]
       else [])
      @ test_selector
    in
    Vdom.Node.div ~attrs [ contents ]
  ;;

  (* As an externally exposed component with no prior style overrides, we don't allow
     opting out of theming to keep user code simpler. *)
  let wrap_with_icon
    ?(extra_attrs = [])
    ?(sort_indicator_attrs = [])
    (label : Vdom.Node.t)
    (sort_state : Bonsai_web_ui_partial_render_table_protocol.Sort_state.t)
    =
    match sort_state with
    | Not_sortable -> Vdom.Node.div [ Vdom.Node.span [ label ] ]
    | _ ->
      let get_arrow = function
        | `Asc -> "▲"
        | `Desc -> "▼"
      in
      let sort_indicator =
        let%map.Option indicator =
          match sort_state with
          | Not_sortable | Not_sorted -> None
          | Single_sort dir -> Some (get_arrow dir)
          | Multi_sort { dir; index } -> Some [%string "%{get_arrow dir} %{index#Int}"]
        in
        Vdom.Node.span ~attrs:sort_indicator_attrs [ Vdom.Node.text indicator ]
      in
      Vdom.Node.div
        ~attrs:
          (Vdom.Attr.style
             (Css_gen.flex_container ~column_gap:(`Px 6) ~align_items:`Baseline ())
           :: extra_attrs)
        [ Vdom.Node.span [ label ]
        ; sort_indicator
          |> Option.value ~default:(Vdom.Node.none_deprecated [@alert "-deprecated"])
        ]
  ;;
end

module Header = struct
  let attr_colspan i =
    match i with
    | 0 -> Vdom.Attr.style (Css_gen.display `None)
    | 1 -> Vdom.Attr.empty
    | i -> Vdom.Attr.create_float "colspan" (Int.to_float i)
  ;;

  module Header_cell = struct
    type t = Vdom.Node.t

    let leaf_view
      (themed_attrs : Themed.t)
      ~column_width
      ~set_column_width
      ~set_column_width_for_reporting
      ~focused
      ~visible
      ~resizable
      ~label
      ~resize_column_widths_to_fit
      ()
      =
      let on_change_tracker =
        match resize_column_widths_to_fit with
        | false ->
          Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
            (fun { border_box = { width; height = _ }; content_box = _ } ->
               set_column_width (`Px_float width))
        | true ->
          (* Set the reporting value so that users of the [Prt.Result.column_widths] field
             get the right results. *)
          Bonsai_web_ui_element_size_hooks.Size_tracker.on_change
            (fun { border_box = { width; height = _ }; content_box = _ } ->
               set_column_width_for_reporting (`Px_float width))
      in
      let node = if resize_column_widths_to_fit then Vdom.Node.th else Vdom.Node.td in
      node
        ~attrs:
          [ themed_attrs.header_cell
          ; (if focused then themed_attrs.header_cell_focused else Vdom.Attr.empty)
          ; on_change_tracker
          ; Vdom.Attr.colspan 1
          ; Functional_style.header_label
          ; Functional_style.leaf_header
          ; (if resizable then Functional_style.leaf_header_resizable else Vdom.Attr.empty)
          ; Vdom.Attr.style
              Css_gen.(width column_width @> if visible then empty else display `None)
          ]
        [ label ]
    ;;

    let spacer_view (themed_attrs : Themed.t) ~colspan ~resize_column_widths_to_fit () =
      let node = if resize_column_widths_to_fit then Vdom.Node.th else Vdom.Node.td in
      node ~attrs:[ themed_attrs.header_cell; attr_colspan colspan ] []
    ;;

    let group_view
      (themed_attrs : Themed.t)
      ~colspan
      ~resize_column_widths_to_fit
      ~label
      ()
      =
      let node = if resize_column_widths_to_fit then Vdom.Node.th else Vdom.Node.td in
      node
        ~attrs:
          [ themed_attrs.header_cell
          ; attr_colspan colspan
          ; Functional_style.header_label
          ]
        [ label ]
    ;;
  end

  module Header_row = struct
    type t = Vdom.Node.t

    let view (themed_attrs : Themed.t) contents =
      Vdom.Node.tr ~attrs:[ themed_attrs.header_row ] contents
    ;;
  end

  type t = Vdom.Node.t

  let view_impl
    (themed_attrs : Themed.t)
    ~set_header_client_rect
    ~resize_column_widths_to_fit
    header_rows
    =
    let attrs =
      [ themed_attrs.header
      ; Bonsai_web_ui_element_size_hooks.Visibility_tracker.detect
          ()
          ~client_rect_changed:set_header_client_rect
      ; Functional_style.partial_render_table_header
      ]
    in
    match resize_column_widths_to_fit with
    | false -> Vdom.Node.table ~attrs [ Vdom.Node.tbody header_rows ]
    | true -> Vdom.Node.thead ~attrs header_rows
  ;;

  (* Fun fact: the header is the only part of partial_render_table that is displayed as an
     actual HTML table!.... unless you're using the table_view *)
  let view
    (themed_attrs : Themed.t)
    ~set_header_client_rect
    ~resize_column_widths_to_fit
    header_rows
    =
    view_impl
      themed_attrs
      ~set_header_client_rect
      ~resize_column_widths_to_fit
      header_rows
  ;;
end

module Cell = struct
  module Col_styles = struct
    (* First index is for the set_or_wrap node, the second index is for the inner wrapper
       for the resize_column_widths_to_fit variant *)
    type t = Vdom.Attr.t list * Vdom.Attr.t list

    (* Css_gen is really slow, so we need to re-use the results of all these functions
       whenever possible. The difference between non-cached and cached css is the
       difference between 200ms stabilizations and 0.2ms stabiliations while scrolling.

       The reason that Css_gen is so slow is because apparently "sprintf" is _really_
       slow. *)
    let create
      (type column_id cmp)
      (module Col_cmp : Comparator.S
        with type t = column_id
         and type comparator_witness = cmp)
      ~(themed_attrs : Themed.t)
      ~resize_column_widths_to_fit
      ~row_height
      ~(col_widths : (column_id, Column_size.t, cmp) Map.t)
      ~(leaves : column_id Header_tree.leaf list)
      =
      let height_styles =
        let h = int_to_px_string row_height in
        Css_gen.(
          create ~field:"height" ~value:h
          @> create ~field:"min-height" ~value:h
          @> create ~field:"max-height" ~value:h)
      in
      let styles_by_column =
        List.fold
          leaves
          ~init:(Map.empty (module Col_cmp))
          ~f:
            (fun
              acc
              { visible = is_visible
              ; column_id
              ; leaf_header = _
              ; initial_width = _
              ; resizable = _
              }
            ->
            match Map.find acc column_id with
            | Some _ -> acc
            | None ->
              let visible_styles =
                match is_visible with
                | false -> Css_gen.display `None
                | true -> Css_gen.empty
              in
              let attrs =
                match resize_column_widths_to_fit with
                | false ->
                  let width_styles =
                    (* We use the previous width even when hidden, so that the rendering
                       engine has less work to do if re-adding a column. Columns that are
                       not currently visible are hidden via `display: None`. *)
                    let w =
                      match Map.find col_widths column_id with
                      | None | Some (Hidden { prev_width_px = None }) -> "0.00px"
                      | Some (Hidden { prev_width_px = Some width })
                      | Some (Visible { width_px = width }) -> float_to_px_string width
                    in
                    Css_gen.(
                      create ~field:"width" ~value:w
                      @> create ~field:"min-width" ~value:w
                      @> create ~field:"max-width" ~value:w)
                  in
                  ( [ Vdom.Attr.style
                        Css_gen.(height_styles @> width_styles @> visible_styles)
                    ; themed_attrs.cell
                    ]
                  , [] )
                | true ->
                  let autosize_cell_styles = {%css|overflow: hidden;|} in
                  (* Height has to be applied to both the wrapper and the inner element *)
                  ( [ themed_attrs.cell; Vdom.Attr.style height_styles ]
                  , [ Vdom.Attr.style Css_gen.(visible_styles @> height_styles)
                    ; autosize_cell_styles
                    ] )
              in
              Map.add_exn acc ~key:column_id ~data:attrs)
      in
      Staged.stage (fun column -> Map.find_exn styles_by_column column)
    ;;
  end

  type t = Vdom.Node.t

  let view
    (themed_attrs : Themed.t)
    ~is_focused
    ~col_styles:(col_styles, wrapper_styles)
    ~on_cell_click
    ~resize_column_widths_to_fit
    content
    =
    let shared_attrs =
      Vdom.Attr.on_click (fun _ -> on_cell_click)
      :: (if is_focused then themed_attrs.cell_focused else Vdom.Attr.empty)
      :: col_styles
    in
    match resize_column_widths_to_fit with
    | false -> set_or_wrap content ~attrs:(Functional_style.cell :: shared_attrs)
    | true ->
      (* In order for the table cell to have a constrained height, we have to wrap the
         content in a div. This is because table cells (div with dislay:table-cell OR td)
         treat the height value as a min-height value, and seem to just ignore max-height

         This wrapping does incur a performance hit. Using chrome profiler, animation
         frame times were:

         With wrapped div: 19-22ms on average, with spikes to 30-32ms and huge (rarer)
         spikes to ~45ms

         Without wrapped div: 14-16 on average, with spikes to 20ms, and rare spikes to
         30ms

         Unfortunately, there's no way of constraining height aside from using this
         wrapping div.

         In the PRT example bonsai, we're diffing ~200 additional elements, but it's
         surprising that that costs so much more time
      *)
      Vdom.Node.div
        ~attrs:
          [ Functional_style.autosize_table_cell_wrapper
          ; themed_attrs.autosize_table_cell_wrapper
          ; (if is_focused
             then themed_attrs.autosize_table_cell_wrapper_focused
             else Vdom.Attr.empty)
          ; {%css|contain: strict;|}
          ]
        [ Vdom.Node.div
            ~attrs:wrapper_styles
            [ set_or_wrap
                ~attrs:(Functional_style.autosize_wrapped_cell :: shared_attrs)
                content
            ]
        ]
  ;;
end

module Row = struct
  module Styles = struct
    type t = Css_gen.t

    let create ~row_height ~row_width ~resize_column_widths_to_fit =
      let h = int_to_px_string row_height in
      let w = float_to_px_string row_width in
      let open Css_gen in
      create ~field:"height" ~value:h
      @>
      match resize_column_widths_to_fit with
      | false -> create ~field:"width" ~value:w @> flex_container ()
      | true -> Css_gen.empty
    ;;
  end

  type t = Vdom.Node.t

  let view
    (themed_attrs : Themed.t)
    ~styles
    ~is_focused
    ~has_focused_cell
    ~extra_attrs
    ~resize_column_widths_to_fit
    cells
    =
    let display_style =
      if resize_column_widths_to_fit
      then Vdom.Attr.style (Css_gen.create ~field:"display" ~value:"table-row")
      else Vdom.Attr.empty
    in
    Vdom.Node.lazy_
      (lazy
        (Vdom.Node.div
           ~attrs:
             (themed_attrs.row
              :: Vdom.Attr.style styles
              :: (if is_focused then themed_attrs.row_focused else Vdom.Attr.empty)
              :: (if has_focused_cell
                  then themed_attrs.row_of_focused_cell
                  else Vdom.Attr.empty)
              :: display_style
              :: extra_attrs)
           cells))
  ;;
end

module Body = struct
  type t = Vdom.Node.t

  module Body_row_key = struct
    module T = struct
      type t =
        | Top_padding
        | Row of Opaque_map.Key.t
        | Bottom_padding
      [@@deriving compare, sexp, equal]
    end

    include Comparable.Make (T)
    include T
  end

  let view_impl (themed_attrs : Themed.t) ~padding_top ~padding_bottom ~rows =
    let style =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.padding_top (`Px padding_top)
           ; Css_gen.padding_bottom (`Px padding_bottom)
           ])
    in
    Vdom.Node.div
      ~attrs:[ themed_attrs.body; style ]
      [ Vdom.Node.Map_children.make ~tag:"div" rows ]
  ;;

  let table_view_impl (themed_attrs : Themed.t) ~padding_top ~padding_bottom ~rows =
    let styles =
      Vdom.Attr.style
        (Css_gen.concat
           [ Css_gen.create ~field:"display" ~value:"table-row-group"
             (* This field is required so that the bottom border element can be positioned
                properly *)
           ; Css_gen.create ~field:"position" ~value:"relative"
           ])
    in
    (*=Because borders don't get collapsed, we only set the top[1] and right borders of
       each row. This makes it difficult to highlight borders of focused rows.

       For every row except for the last, we can use the "next sibling" selector to set the
       top border (which acts as the current row's bottom border). The last row has no
       next sibling, but we can quickly access it using the ":last-child" selector.

       Auto-resizing tables have spacing divs at the top / bottom [3], so the last row isn't
       the `:last-child`. But the last row is visible iff we are at the table, in which
       case, the bottom spacer isn't needed, so we don't insert it, and `:last-child`

      -------
      [1] We need to set top, not bottom, because there's a `+` "next sibling" selector
       that we can use to highlight the top border of the next row. There's no "previous
       sibling" selector[2], so we can't highlight the bottom border of the previous row.
      [2] we could simulate one with `:has(+ ...)`, but that's terribly slow.
      [3] Initially this was done with ppx_css and css pseudoelements, but something about
       adding the attr to the top-level div and changing the cssvars made things a lot
       slower. Could possibly be due to how the diffing is done for the top-level element,
       or may be something to do with the browser implementation of updating that var, but
       using child elements is significantly faster as of now.
    *)
    let bottom_padding =
      if padding_bottom = 0
      then Vdom.Node.none
      else
        Vdom.Node.div
          ~key:"bottom_padding"
          ~attrs:[ Vdom.Attr.style (Css_gen.height (`Px padding_bottom)) ]
          []
    in
    let rows =
      Map.set
        rows
        ~key:Body_row_key.Top_padding
        ~data:
          (Vdom.Node.div
             ~key:"top_padding"
             ~attrs:[ Vdom.Attr.style (Css_gen.height (`Px padding_top)) ]
             [])
      |> Map.set ~key:Body_row_key.Bottom_padding ~data:bottom_padding
    in
    Vdom.Node.Map_children.make
      ~tag:"div"
      ~attr:Vdom.Attr.(styles @ themed_attrs.body)
      rows
  ;;

  let view themed_attrs ~padding_top ~padding_bottom ~rows ~resize_column_widths_to_fit =
    Vdom.Node.lazy_
      (lazy
        ((match resize_column_widths_to_fit with
          | false -> view_impl
          | true -> table_view_impl)
           themed_attrs
           ~padding_top
           ~padding_bottom
           ~rows))
  ;;
end

module Table = struct
  let view
    (themed_attrs : Themed.t)
    ~private_body_classname
    ~vis_change_attr
    ~header_height
    ~rows_height
    ~resize_column_widths_to_fit
    head
    body
    =
    (* If the number is large enough, it will use scientific notation for unknown reasons.
       However, the number is accurate, and scientific notation is in spec.
       https://developer.mozilla.org/en-US/docs/Web/CSS/number *)
    let inner_container_attrs =
      let total_height =
        Float.of_int rows_height
        +. if resize_column_widths_to_fit then header_height else 0.
      in
      [ Vdom.Attr.class_ private_body_classname
      ; Vdom.Attr.style Css_gen.(height (`Px_float total_height))
        (* This attr determines where the visible client rect is for the body. If the
           structure of the elements it is placed on changes, row index/scroll position
           calculation may need to be corrected. *)
      ; vis_change_attr
      ]
    in
    let children =
      match resize_column_widths_to_fit with
      | false ->
        [ head
        ; Vdom.Node.div
            ~attrs:
              (Functional_style.default_partial_render_table_body :: inner_container_attrs)
            [ body ]
        ]
      | true -> [ Vdom.Node.div ~attrs:inner_container_attrs [ head; body ] ]
    in
    Vdom.Node.div
      ~attrs:
        [ themed_attrs.table_vars
        ; themed_attrs.table
        ; Functional_style.partial_render_table_container
        ]
      children
  ;;
end
