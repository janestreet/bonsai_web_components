open! Core
open! Bonsai_web
open Bonsai_bench_scenario

module Input = struct
  type ('key, 'data, 'cmp) t =
    { filter : (key:'key -> data:'data -> bool) option
    ; order : ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    ; rank_range : int Incr_map_collate.Collate_params.Which_range.t
    ; key_range : 'key Incr_map_collate.Collate_params.Which_range.t
    ; resize_column_widths_to_fit : bool
    ; row_height : [ `Px of int ]
    ; map : ('key, 'data, 'cmp) Map.t
    }

  let create
    ?(filter = None)
    ?(order = Incr_map_collate.Compare.Unchanged)
    ?(rank_range = Incr_map_collate.Collate_params.Which_range.To 100)
    ?(key_range = Incr_map_collate.Collate_params.Which_range.All_rows)
    ?(resize_column_widths_to_fit = false)
    ?(row_height = `Px 30)
    map
    =
    { filter; order; rank_range; key_range; resize_column_widths_to_fit; map; row_height }
  ;;

  let apply_filter t filter =
    Interaction.update_input t ~f:(fun x -> { x with filter = Some filter })
  ;;

  let clear_filter t = Interaction.update_input t ~f:(fun x -> { x with filter = None })
  let update_map t ~f = Interaction.update_input t ~f:(fun x -> { x with map = f x.map })
  let set_order t order = Interaction.update_input t ~f:(fun x -> { x with order })

  let set_rank_range t rank_range =
    Interaction.update_input t ~f:(fun x -> { x with rank_range })
  ;;

  let scroll t ~start ~stop ~window_size =
    let stride = if start > stop then -1 else 1 in
    List.range ~stride start stop
    |> List.map ~f:(fun i ->
      set_rank_range
        t
        (Incr_map_collate.Collate_params.Which_range.Between (i, i + window_size - 1)))
    |> Interaction.many_with_recomputes
  ;;
end

module Navigation_action = struct
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
