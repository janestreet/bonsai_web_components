open! Core
open! Byo_bonsai_prelude

type t =
  { name : string
  ; svg_path : string
  ; view_box_dimension : int
  ; (* Determines which property is use to set the icon's color. *)
    color_property : [ `Stroke | `Fill ]
  }

let name t = t.name

let render_test_icon ?size ?color ?stroke_width ~attrs (t : t) =
  let test_attr ~name ~to_string value =
    Option.value_map value ~default:Attr.empty ~f:(fun v ->
      Attr.create name (to_string v))
  in
  let attrs =
    [ test_attr ~name:"size" ~to_string:Css_gen.Length.to_string_css size
    ; test_attr ~name:"color" ~to_string:Css_gen.Color.to_string_css color
    ; test_attr ~name:"stroke-width" ~to_string:Css_gen.Length.to_string_css stroke_width
    ; Attr.many attrs
    ]
  in
  Node.create ~attrs [%string "icon-%{t.name}"] []
;;

let size_or_default size =
  match size with
  | Some size -> (size :> Css_gen.Length.t)
  | None -> `Px 16
;;

let stroke_or_default t color =
  Css_gen.Color.to_string_css
    (match t.color_property, color with
     | `Stroke, Some stroke -> (stroke :> Css_gen.Color.t)
     | `Stroke, None -> `Name "currentColor"
     | `Fill, _ -> `Name "none")
;;

let fill_or_default t color =
  Css_gen.Color.to_string_css
    (match t.color_property, color with
     | `Fill, Some fill -> (fill :> Css_gen.Color.t)
     | `Fill, None -> `Name "currentColor"
     | `Stroke, _ -> `Name "none")
;;

let stroke_width_or_default stroke_width =
  Css_gen.Length.to_string_css
    (match stroke_width with
     | Some stroke_width -> (stroke_width :> Css_gen.Length.t)
     | None -> `Px 2)
;;

let view_box t = [%string "0 0 %{t.view_box_dimension#Int} %{t.view_box_dimension#Int}"]

let render_icon ?size ?color ?stroke_width ?(attrs = []) (icon : t) =
  let size = size_or_default size in
  let attrs =
    Attr.
      [ Attr.style (Css_gen.width size)
      ; Attr.style (Css_gen.height size)
      ; create "viewBox" (view_box icon)
      ; create "stroke" (stroke_or_default icon color)
      ; create "stroke-width" (stroke_width_or_default stroke_width)
      ; create "stroke-linecap" "round"
      ; create "stroke-linejoin" "round"
      ; create "fill" (fill_or_default icon color)
      ; (* When using an icon inside a flexbox container, you almost certainly want this
           so that the icon is not squished. *)
        Attr.style (Css_gen.create ~field:"flex-shrink" ~value:"0")
      ; (* Some icons are wierdly clipped by 1px it this is not set. *)
        Attr.style (Css_gen.overflow `Visible)
      ; many attrs
      ]
  in
  Node.inner_html_svg
    ~tag:"svg"
    ~attrs
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:icon.svg_path
    ()
;;

let view ?size ?color ?stroke_width ?(attrs = []) ~(icon : t) () =
  match Am_running_how_js.am_running_how with
  | `Node_test | `Node_jsdom_test ->
    render_test_icon ?size ?color ?stroke_width ~attrs icon
  | `Browser | `Browser_test | `Browser_benchmark | `Node | `Node_benchmark ->
    render_icon ?size ?color ?stroke_width ~attrs icon
;;

module Expert = struct
  let create ~name ~svg_path ~view_box_dimension ~color_property =
    { name; svg_path; view_box_dimension; color_property }
  ;;

  let svg_string ?size ?color ?stroke_width ~(icon : t) () =
    let size = size_or_default size in
    {%string|
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="%{Css_gen.Length.to_string_css size}"
      height="%{Css_gen.Length.to_string_css size}"
      viewBox="%{view_box icon}"
      stroke="%{stroke_or_default icon color}"
      stroke-width="%{stroke_width_or_default stroke_width}"
      stroke-linecap="round"
      stroke-linejoin="round"
      fill="%{fill_or_default icon color}"
      >
        %{icon.svg_path}
    </svg>
    |}
  ;;
end

module For_testing = struct
  let render_icon = render_icon
end
