open! Core
open! Bonsai_web
open Bonsai_bench_scenario

(** The tools in the [Sharable] module are intended to be usable for any PRT. *)

module Input : sig
  (** An [Input.t] packages up all of the inputs to the partial render table and provides
      facilities for modifying individual components. *)
  type ('key, 'data, 'cmp) t =
    { filter : (key:'key -> data:'data -> bool) option
    ; order : ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    ; rank_range : int Incr_map_collate.Collate_params.Which_range.t
    ; key_range : 'key Incr_map_collate.Collate_params.Which_range.t
    ; resize_column_widths_to_fit : bool
    ; row_height : [ `Px of int ]
    ; map : ('key, 'data, 'cmp) Map.t
    }

  (** [create] produces a [t], with defaults for most components of the input.

      [filter] defaults to [None] [order] defaults to [Compare.Unchanged] [rank_range]
      defaults to [Which_range.To 100] [key_range] defaults to [Which_range.All_rows]
      [resize_column_widths_to_fit] defaults to [false] [row_height] defaults to [`Px 30] *)
  val create
    :  ?filter:(key:'a -> data:'b -> bool) option
    -> ?order:('a, 'b, 'c) Incr_map_collate.Compare.t
    -> ?rank_range:int Incr_map_collate.Collate_params.Which_range.t
    -> ?key_range:'a Incr_map_collate.Collate_params.Which_range.t
    -> ?resize_column_widths_to_fit:bool
    -> ?row_height:[ `Px of int ]
    -> ('a, 'b, 'c) Base.Map.t
    -> ('a, 'b, 'c) t

  (** [apply_filter] produces an interaction to change the current filter. *)
  val apply_filter
    :  ('key, 'data, 'cmp) t Input.t
    -> (key:'key -> data:'data -> bool)
    -> 'action Interaction.t

  (** [clear_filter] produces an interaction to remove the current filter. *)
  val clear_filter : _ t Input.t -> 'action Interaction.t

  (** [update_map] produces an interaction to change the map whose data is being rendered
      in the table. *)
  val update_map
    :  ('key, 'data, 'cmp) t Input.t
    -> f:(('key, 'data, 'cmp) Map.t -> ('key, 'data, 'cmp) Map.t)
    -> 'action Interaction.t

  (** [set_order] produces an interaction to change the current ordering. *)
  val set_order
    :  ('key, 'data, 'cmp) t Input.t
    -> ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    -> 'action Interaction.t

  (** [set_rank_range] produces an interaction to change the currently visible rank range. *)
  val set_rank_range
    :  _ t Input.t
    -> int Incr_map_collate.Collate_params.Which_range.t
    -> 'action Interaction.t

  (** [scroll] generates an interaction with abs(start-stop) [change_input]s, which set
      the [rank_range]'s low end to the values between [start] (inclusive) and [stop]
      (exclusive), keeping [window_size] elements in the range. *)
  val scroll
    :  _ t Input.t
    -> start:int
    -> stop:int
    -> window_size:int
    -> 'action Interaction.t
end

module Navigation_action : sig
  (** An [Navigation_action.t] represents the common actions used for navigating around /
      focusing a PRT. *)
  type 'key t =
    | Unfocus
    | Focus_up
    | Focus_down
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus_first_column of 'key
    | Focus_index_first_column of int
  [@@deriving sexp, equal]
end
