open! Core
open! Virtual_dom
include Svg

let test_attr ~name ~value ~to_string =
  Option.value_map value ~default:Vdom.Attr.empty ~f:(fun v ->
    Vdom.Attr.create name (to_string v))
;;

let name t =
  Svg.sexp_of_t t
  |> Sexp.to_string
  |> String.lowercase
  |> String.map ~f:(function
    | '_' -> '-'
    | c -> c)
;;

let svg ?size ?color ?(extra_attrs = []) icon =
  let override_vdom_for_testing =
    lazy
      (let attr =
         Vdom.Attr.many
           [ test_attr ~name:"size" ~value:size ~to_string:Css_gen.Length.to_string_css
           ; test_attr ~name:"color" ~value:color ~to_string:Css_gen.Color.to_string_css
           ; Vdom.Attr.many extra_attrs
           ]
       in
       Vdom.Node.create (sprintf "codicon-%s" (name icon)) [] ~attrs:[ attr ])
  in
  let size =
    match size with
    | Some size -> Css_gen.Length.to_string_css (size :> Css_gen.Length.t)
    | None -> "16px"
  in
  let color =
    match color with
    | None -> `Name "currentColor"
    | Some color -> (color :> Css_gen.Color.t)
  in
  let view_box =
    let frame = Svg.frame icon in
    [%string "0 0 %{frame#Int} %{frame#Int}"]
  in
  let attr =
    Vdom.Attr.(
      many
        [ create "width" size
        ; create "height" size
        ; create "viewBox" view_box
        ; create "fill" (Css_gen.Color.to_string_css color)
        ; (* When using an icon inside a flexbox container, you almost certainly want this
             so that the icon is not squished. *)
          Vdom.Attr.style (Css_gen.create ~field:"flex-shrink" ~value:"0")
        ; (* Some icons are wiredly clipped by 1px it this is not set. *)
          Vdom.Attr.style (Css_gen.overflow `Visible)
        ; many extra_attrs
        ])
  in
  Vdom.Node.inner_html_svg
    ~override_vdom_for_testing
    ~tag:"svg"
    ~attrs:[ attr ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:(Svg.svg icon)
    ()
;;

let all = Svg.all

let to_bonsai_web_icon t =
  Bonsai_web_icon.Expert.create
    ~name:(name t)
    ~svg_path:(Svg.svg t)
    ~view_box_dimension:(frame t)
    ~color_property:`Fill
;;
