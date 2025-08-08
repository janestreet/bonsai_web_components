open! Core
open! Import

include module type of struct
  include Feather_icon_common
end

(** Useful for favicons and css backgrounds *)
val svg_string
  :  ?size:[< Css_gen.Length.t ]
  -> ?stroke:[< Css_gen.Color.t ]
  -> ?fill:[< Css_gen.Color.t ]
  -> ?stroke_width:[< Css_gen.Length.t ]
  -> t
  -> string

(* $MDX part-begin=svg *)

val svg
  :  ?size:[< Css_gen.Length.t ]
  -> ?stroke:[< Css_gen.Color.t ]
  -> ?fill:[< Css_gen.Color.t ]
  -> ?stroke_width:[< Css_gen.Length.t ]
  -> ?extra_attrs:Vdom.Attr.t list
  -> t
  -> Vdom.Node.t

(* $MDX part-end *)

val to_string : t -> string
