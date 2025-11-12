open! Core
open! Byo_bonsai_prelude

(** A [Byo_icon] is an abstract wrapper around an svg icon. It's designed so that
    third-party icon sets such as Lucide, Feather, and Codicons can be imported to our
    ecosystem using the same underlying representation. Additionally, it allows us to
    create our own internal icon sets that are represented the same way. *)
type t

(** Render a given [Byo_icon.t] as a [Node.t].

    In [Node_test] and [Node_jsdom_test] a test node will be rendered instead of the svg. *)
val view
  :  ?size:[< Css_gen.Length.t ]
  -> ?color:[< Css_gen.Color.t ]
  -> ?stroke_width:[< Css_gen.Length.t ]
  -> ?attrs:Attr.t list
  -> icon:t
  -> unit
  -> Node.t

(** Get the icon's name. *)
val name : t -> string

(** Functions in [Expert] can produce unsafe code due to the conversion of strings to
    [Vdom]. Use with caution, and reach out to [#discuss-webdev] if unsure. *)
module Expert : sig
  (** Create a [Byo_icon]. *)
  val create
    :  name:string
    -> svg_path:string
    -> view_box_dimension:int
    -> color_property:[ `Fill | `Stroke ]
    -> t

  (** Render a svg as a string. This can be useful for favicons or including inside css,
      e.g. as a custom background. *)
  val svg_string
    :  ?size:[< Css_gen.Length.t ]
    -> ?color:[< Css_gen.Color.t ]
    -> ?stroke_width:[< Css_gen.Length.t ]
    -> icon:t
    -> unit
    -> string
end

module For_testing : sig
  val render_icon
    :  ?size:[< Css_gen.Length.t ]
    -> ?color:[< Css_gen.Color.t ]
    -> ?stroke_width:[< Css_gen.Length.t ]
    -> ?attrs:Attr.t list
    -> t
    -> Node.t
end
