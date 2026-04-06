open! Core
open! Virtual_dom
include Generated.Icon

let name icon =
  sexp_of_t icon
  |> Sexp.to_string
  |> String.lowercase
  |> String.map ~f:(function
    | '_' -> '-'
    | c -> c)
;;

let svg
  ?(size = (`Px 24 :> Css_gen.Length.t))
  ?(color = (`Name "currentColor" :> Css_gen.Color.t))
  ?(attrs = [])
  icon
  =
  let size = Css_gen.Length.to_string_css size in
  let color = Css_gen.Color.to_string_css color in
  let override_vdom_for_testing =
    lazy
      (let attr =
         Vdom.Attr.many
           [ Vdom.Attr.create "size" size
           ; Vdom.Attr.create "color" color
           ; Vdom.Attr.many attrs
           ]
       in
       Vdom.Node.create (sprintf "seti-icon-%s" (name icon)) [] ~attrs:[ attr ])
  in
  let svg_attrs, svg_content = Generated.svg icon in
  let attr =
    Vdom.Attr.(
      many
        [ string_property "width" size
        ; string_property "height" size
        ; string_property "fill" color
        ; (* When using an icon inside a flexbox container, you almost certainly want this
             so that the icon is not squished. *)
          Vdom.Attr.style (Css_gen.create ~field:"flex-shrink" ~value:"0")
        ; (* Some icons are wiredly clipped by 1px it this is not set. *)
          Vdom.Attr.style (Css_gen.overflow `Visible)
        ; many (List.map svg_attrs ~f:(fun (name, value) -> string_property name value))
        ; many attrs
        ])
  in
  Vdom.Node.inner_html_svg
    ~override_vdom_for_testing
    ~tag:"svg"
    ~attrs:[ attr ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:svg_content
    ()
;;

let by_file_extension = String.Map.of_alist_exn Generated.mapping
