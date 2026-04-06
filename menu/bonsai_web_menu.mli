open! Core
open! Bonsai_web

module Item : sig
  type ('effect, 'item) t =
    | Single of
        { key : string
        ; disabled : bool
        ; on_click : 'effect Effect.t
        ; item : 'item
        }
    | Section of
        { title : string option
        ; items : ('effect, 'item) t list
        }
    | Inert of 'item
    | Submenu of
        { key : string
        ; item : 'item
        ; items : ('effect, 'item) t list
        }
  [@@deriving sexp_of]

  val map_actions : ('a, 'item) t -> f:('a Effect.t -> 'b Effect.t) -> ('b, 'item) t

  val find_map
    :  ('effect, 'item) t list
    -> f:(('effect, 'item) t option -> ('effect, 'item) t -> 'b option)
    -> 'b option

  val find_submenu : ('effect, 'item) t list -> string list -> ('effect, 'item) t list
  val first : ('effect, 'item) t list -> ('effect, 'item) t option
  val last : ('effect, 'item) t list -> ('effect, 'item) t option
end

type 'item t

val component
  :  (unit, 'item) Item.t list Bonsai.t
  -> local_ Bonsai.graph
  -> 'item t Bonsai.t

val active_path : 'item t -> string list
val active_item : 'item t -> (unit, 'item) Item.t option Effect.t
val set_active_path : 'item t -> string list -> unit Effect.t
val key_down : 'item t -> [ `Enter | `Up | `Down | `Left | `Right ] -> unit Effect.t
