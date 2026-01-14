open! Core
include module type of Custom_ojs_converter
include module type of For_ppx

(* These extra modules are just here to provide a more intuitive interface for the raw
   CodeMirror bindings. *)

module type State_effect_spec = sig
  type t
end

module State : sig
  include module type of For_ppx.State

  module State_effect : sig
    type t = State.state_effect

    val is : t -> type_:'a State_effect_type.t -> bool
    val value : t -> type_:'a State.State_effect_type.t -> 'a option

    (** The "real" type of `State_effect.define` is `unit -> 'a State_effect_type.t`. We
        require passing in a First Class Module with [type t = 'a] so that we only create
        state effects connected to real types. *)
    val define : (module State_effect_spec with type t = 'a) -> 'a State_effect_type.t

    val reconfigure : Extension.t State_effect_type.t
  end

  module Range_set_update_spec : sig
    include module type of Range_set_update_spec

    module Filter_spec : sig
      (* Filters the ranges already in the set between [from] and [to_]. Only ranges for
         which [f] returns true are kept. *)
      type 'v t =
        { f : from:int -> to_:int -> value:'v -> bool
        ; filter_from : int
        ; filter_to : int
        }
    end

    val create
      :  add:'v Range.t list
      -> sort:bool
      -> filter:'v Filter_spec.t option
      -> 'v t
  end

  module Range_set : sig
    include module type of Range_set

    val between
      :  'v t
      -> from:int
      -> to_:int
      -> f:(from:int -> to_:int -> value:'v -> [ `Stop | `Continue ])
      -> unit
  end
end

module Gutter : sig
  include module type of For_ppx.Gutter

  module Gutter_marker_type : sig
    open Js_of_ocaml

    (** [define] should be called only once per "kind" of gutter marker you want to
        create; consider using it in a global lazy.

        This is because the Codemirror API requires an abstract class for gutter markers.
        Presumably this allows Codemirror to use [instanceof], so it only compares gutter
        markers of the same "kind".

        If all of your gutter markers of the same "kind" should be the same, it's ok to
        just have a global singleton value:
        https://codemirror.net/docs/ref/#view.GutterMarker *)
    val define
      :  to_dom:('input -> View.Editor_view.t -> Dom_html.element Js.t)
      -> ?destroy:('input -> Dom_html.element Js.t -> unit)
      -> ?equal:('input -> 'input -> bool)
      -> ?element_class:string
      -> unit
      -> ('input -> Gutter_marker.t) Staged.t
  end
end
