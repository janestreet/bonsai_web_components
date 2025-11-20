open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Sort_kind = Column_intf.Sort_kind

module Indexed_column_id = struct
  type t = int [@@deriving sexp, equal]

  let to_int = Fn.id
  let of_int = Fn.id
end

module type Extra = sig
  type ('key, 'data) t
end

module Dynamic_cells_shared (Extra : Extra) = struct
  type ('key, 'data) t =
    | Leaf of
        { leaf_header : Vdom.Node.t Bonsai.t
        ; initial_width : Css_gen.Length.t
        ; cell :
            key:'key Bonsai.t
            -> data:'data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t
        ; visible : bool Bonsai.t
        ; resizable : bool Bonsai.t
        ; extra : ('key, 'data) Extra.t Bonsai.t
        }
    | Group of
        { children : ('key, 'data) t list
        ; group_header : Vdom.Node.t Bonsai.t
        }
    | Org_group of ('key, 'data) t list

  let headers ~wrap_header t (local_ _graph) =
    let rec loop ~next_id = function
      | Leaf { leaf_header; visible; initial_width; cell = _; resizable; extra = _ } ->
        let tree =
          let%arr header = leaf_header
          and visible
          and resizable
          and wrap_header in
          let column_id = Indexed_column_id.of_int next_id in
          Header_tree.leaf
            ~header:(wrap_header ~column_id header)
            ~visible
            ~initial_width
            ~resizable
            ~column_id
        in
        tree, next_id + 1
      | Group { children; group_header } ->
        let next_id, children =
          List.fold_map ~init:next_id children ~f:(fun next_id child ->
            let child, next_id = loop ~next_id child in
            next_id, child)
        in
        let tree =
          let%arr header = group_header
          and children = Bonsai.all children in
          Header_tree.group ~header children
        in
        tree, next_id
      | Org_group children ->
        let next_id, children =
          List.fold_map ~init:next_id children ~f:(fun next_id child ->
            let child, next_id = loop ~next_id child in
            next_id, child)
        in
        let tree =
          let%arr children = Bonsai.all children in
          Header_tree.org_group children
        in
        tree, next_id
    in
    let tree, _ = loop ~next_id:0 t in
    tree
  ;;

  let visible_leaves map comparator t =
    let rec loop
      : type k v cmp.
        next_id:int
        -> (k * v) Opaque_map.t Bonsai.t
        -> (k, cmp) Comparator.Module.t
        -> (k, v) t
        -> ((local_ Bonsai.graph -> (k * Vdom.Node.t) Opaque_map.t Bonsai.t)
           * Indexed_column_id.t)
             list
           * int
      =
      fun ~next_id map comparator -> function
      | Leaf { cell; visible; _ } ->
        let leaf (local_ graph) =
          if%sub visible
          then
            Bonsai.Expert.assoc_on
              (module Opaque_map.Key)
              comparator
              map
              ~get_model_key:(fun _ (k, _) -> k)
              ~f:(fun _ data (local_ graph) ->
                let%sub key, data = data in
                let r = cell ~key ~data graph in
                let%arr key and r in
                key, r)
              graph
          else (
            let f =
              Ui_incr.Map.map ~f:(fun (k, _) ->
                k, (Vdom.Node.none_deprecated [@alert "-deprecated"]))
            in
            Bonsai.Incr.compute map ~f graph)
        in
        [ leaf, Indexed_column_id.of_int next_id ], next_id + 1
      | Group { children; _ } | Org_group children ->
        let next_id, leaves =
          List.fold_map children ~init:next_id ~f:(fun next_id child ->
            let leaves, next_id = loop ~next_id map comparator child in
            next_id, leaves)
        in
        let leaves = List.concat leaves in
        leaves, next_id
    in
    let leaves, _ = loop ~next_id:0 map comparator t in
    leaves
  ;;

  let instantiate_cells (type k) t comparator (map : (k * _) Opaque_map.t Bonsai.t) =
    let empty = Map.empty (module Opaque_map.Key) in
    visible_leaves map comparator t
    |> List.fold_right
         ~init:(fun (local_ _graph) -> Bonsai.return empty)
         ~f:(fun (leaf_comp, column_id) acc (local_ graph) ->
           let a = leaf_comp graph in
           let acc = acc graph in
           Bonsai.Incr.compute (Bonsai.both a acc) graph ~f:(fun a_and_acc ->
             let%pattern_bind.Ui_incr a, acc = a_and_acc in
             Ui_incr.Map.merge a acc ~f:(fun ~key:_ change ->
               match change with
               | `Left (i, l) -> Some (i, [ column_id, l ])
               | `Right (i, r) -> Some (i, r)
               | `Both ((i, l), (_, r)) -> Some (i, (column_id, l) :: r))))
  ;;
end

module Dynamic_columns_shared (Extra : Extra) = struct
  type ('key, 'data) t =
    | Leaf of
        { leaf_header : Vdom.Node.t
        ; initial_width : Css_gen.Length.t
        ; cell : key:'key -> data:'data -> Vdom.Node.t
        ; visible : bool
        ; resizable : bool
        ; extra : ('key, 'data) Extra.t
        }
    | Group of
        { children : ('key, 'data) t list
        ; group_header : Vdom.Node.t
        }
    | Org_group of ('key, 'data) t list

  let headers' ~wrap_header t =
    let rec map_children ~next_id children =
      List.fold_map children ~init:next_id ~f:(fun next_id child ->
        let tree, next_id = loop ~next_id child in
        next_id, tree)
    and loop ~next_id = function
      | Leaf
          { leaf_header = header; initial_width; visible; cell = _; resizable; extra = _ }
        ->
        let column_id = Indexed_column_id.of_int next_id in
        ( Header_tree.leaf
            ~header:(wrap_header ~column_id header)
            ~visible
            ~resizable
            ~initial_width
            ~column_id
        , next_id + 1 )
      | Group { children; group_header = header } ->
        let next_id, tree = map_children ~next_id children in
        Header_tree.group ~header tree, next_id
      | Org_group children ->
        let next_id, tree = map_children ~next_id children in
        Header_tree.org_group tree, next_id
    in
    let tree, _ = loop ~next_id:0 t in
    tree
  ;;

  let headers ~wrap_header t (local_ _graph) =
    let%arr t and wrap_header in
    headers' ~wrap_header t
  ;;

  let visible_leaves structure ~key ~data =
    let rec loop ~next_id structure ~key ~data =
      match structure with
      | Leaf { cell; visible; _ } ->
        let leaf =
          if visible
          then [ Indexed_column_id.of_int next_id, cell ~key ~data ]
          else
            [ ( Indexed_column_id.of_int next_id
              , (Vdom.Node.none_deprecated [@alert "-deprecated"]) )
            ]
        in
        leaf, next_id + 1
      | Org_group children | Group { children; group_header = _ } ->
        let next_id, leaves =
          List.fold_map children ~init:next_id ~f:(fun next_id child ->
            let leaves, next_id = loop child ~next_id ~key ~data in
            next_id, leaves)
        in
        let leaves = List.concat leaves in
        leaves, next_id
    in
    let leaves, _ = loop ~next_id:0 structure ~key ~data in
    leaves
  ;;

  let instantiate_cells t _comparator map (local_ graph) =
    Bonsai.Incr.compute (Bonsai.both t map) graph ~f:(fun both ->
      let%pattern_bind.Ui_incr t, map = both in
      (* Why is this bind here ok? Well, there is an alternative that involves
         Incr_map.mapi' which closes over visible_leaves as an incremental, but even in
         that scenario, if the set of visible_leaves changes, we're recomputing the whole
         world anyway, so it doesn't buy us anything vs this bind. *)
      let%bind.Ui_incr visible_leaves = Ui_incr.map t ~f:visible_leaves in
      Ui_incr.Map.map map ~f:(fun (key, data) -> key, visible_leaves ~key ~data))
  ;;
end

module Extra_server_side = struct
  type ('key, 'data) t = unit
end

module Dynamic_cells = struct
  module T = Dynamic_cells_shared (Extra_server_side)

  type ('key, 'data) t = ('key, 'data) T.t

  let column
    ?(initial_width = Columns.Column_structure.default_initial_width)
    ?(visible = Bonsai.return true)
    ?(resizable = Bonsai.return true)
    ~header
    ~cell
    ()
    =
    T.Leaf
      { leaf_header = header; initial_width; cell; visible; resizable; extra = return () }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  let lift
    : type key data.
      (key, data) T.t list -> (key, data, Indexed_column_id.t) Column_intf.t
    =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = Indexed_column_id.t

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value = T.Org_group columns in
      Column_intf.T { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_experimental = struct
  module T = struct
    type ('key, 'data, 'column_id, 'column_id_cmp) t =
      { column_id : ('column_id, 'column_id_cmp) Comparator.Module.t
      ; columns : 'column_id list Bonsai.t
      ; columns_as_a_set : ('column_id, 'column_id_cmp) Set.t Bonsai.t
      ; render_header : 'column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
      ; render_cell :
          'column_id Bonsai.t
          -> 'key Bonsai.t
          -> 'data Bonsai.t
          -> local_ Bonsai.graph
          -> Vdom.Node.t Bonsai.t
      }

    let headers
      : type key data column_id column_id_cmp.
        wrap_header:(column_id:column_id -> Vdom.Node.t -> Vdom.Node.t) Bonsai.t
        -> (key, data, column_id, column_id_cmp) t
        -> local_ Bonsai.graph
        -> column_id Header_tree.t Bonsai.t
      =
      fun ~wrap_header
        { column_id; columns; columns_as_a_set = _; render_header; render_cell = _ }
        (local_ graph) ->
      let module Col_id = (val column_id) in
      let rendered =
        Bonsai.assoc_list
          (module Col_id)
          columns
          ~get_key:Fn.id
          ~f:(fun _ column_id (local_ graph) ->
            let header = render_header column_id graph in
            let%arr header and column_id and wrap_header in
            Header_tree.leaf
              ~header:(wrap_header ~column_id header)
              ~visible:true
              ~initial_width:Columns.Column_structure.default_initial_width
              ~column_id
              ~resizable:true)
          graph
      in
      match%sub rendered with
      | `Duplicate_key _ ->
        raise_s
          [%message
            "BUG" [%here] "should be impossible because columns were already deduplicated"]
      | `Ok headers ->
        let%arr headers in
        Header_tree.org_group headers
    ;;

    let instantiate_cells
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> (key, _) Comparator.Module.t
        -> (key * data) Opaque_map.t Bonsai.t
        -> local_ Bonsai.graph
        -> (key * (column_id * Vdom.Node.t) list) Opaque_map.t Bonsai.t
      =
      fun { column_id; columns; columns_as_a_set = _; render_header = _; render_cell }
        key_cmp
        rows ->
      let module Col_id = (val column_id) in
      Bonsai.Expert.assoc_on
        (module Opaque_map.Key)
        key_cmp
        rows
        ~get_model_key:(fun _ (k, _) -> k)
        ~f:(fun _key data (local_ graph) ->
          let%sub key, data = data in
          let rendered =
            Bonsai.assoc_list
              (module Col_id)
              columns
              ~get_key:Fn.id
              ~f:(fun _ col (local_ graph) ->
                let cell = render_cell col key data graph in
                let%arr col and cell in
                col, cell)
              graph
          in
          match%sub rendered with
          | `Duplicate_key _ ->
            raise_s
              [%message
                "BUG"
                  [%here]
                  "should be impossible because columns were already deduplicated"]
          | `Ok cells ->
            let%arr cells and key in
            key, cells)
    ;;
  end

  let dedup
    (type column column_cmp)
    columns
    (column_id : (column, column_cmp) Comparator.Module.t)
    =
    let module Col_id = (val column_id) in
    let%sub columns, columns_as_a_set =
      let%arr columns in
      (* deduplicate *)
      let seen = ref (Set.empty (module Col_id)) in
      let as_list =
        List.filter columns ~f:(fun column ->
          if Set.mem !seen column
          then false
          else (
            seen := Set.add !seen column;
            true))
      in
      as_list, !seen
    in
    columns, columns_as_a_set
  ;;

  let build
    : type key data column column_cmp.
      (column, column_cmp) Comparator.Module.t
      -> columns:column list Bonsai.t
      -> render_header:(column Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:
           (column Bonsai.t
            -> key Bonsai.t
            -> data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
      -> (key, data, column) Column_intf.t
    =
    let module X = struct
      type t = (key, data, column, column_cmp) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = column

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun column_id ~columns ~render_header ~render_cell ->
      let columns, columns_as_a_set = dedup columns column_id in
      let value =
        { T.column_id; columns; columns_as_a_set; render_header; render_cell }
      in
      Column_intf.T { value; vtable = (module X); column_id }
  ;;

  module Sortable = Sortable
end

module Dynamic_columns = struct
  module T = Dynamic_columns_shared (Extra_server_side)

  type ('key, 'data) t = ('key, 'data) T.t

  let column
    ?(initial_width = Columns.Column_structure.default_initial_width)
    ?(visible = true)
    ?(resizable = true)
    ~header
    ~cell
    ()
    =
    T.Leaf { leaf_header = header; initial_width; cell; visible; resizable; extra = () }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  let lift
    : type key data.
      (key, data) T.t list Bonsai.t -> (key, data, Indexed_column_id.t) Column_intf.t
    =
    let module X = struct
      type t = (key, data) T.t Bonsai.t
      type nonrec key = key
      type nonrec data = data
      type column_id = Indexed_column_id.t

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun columns ->
      let value =
        let%arr columns in
        T.Org_group columns
      in
      Column_intf.T { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Extra_client_side = struct
  type ('key, 'data) t = ('key, 'data) Sort_kind.t option
end

module Dynamic_cells_with_sorter = struct
  module T = Dynamic_cells_shared (Extra_client_side)

  type ('key, 'data) t = ('key, 'data) T.t

  let column
    ?sort
    ?sort_reversed
    ?(initial_width = Columns.Column_structure.default_initial_width)
    ?(visible = Bonsai.return true)
    ?(resizable = Bonsai.return true)
    ~header
    ~cell
    ()
    =
    let sort =
      match sort, sort_reversed with
      | None, None -> Bonsai.return None
      | Some forward, Some reverse ->
        let%arr forward and reverse in
        Some { Sort_kind.forward; reverse }
      | Some forward, None ->
        let%arr forward in
        Some (Sort_kind.reversible ~forward)
      | None, Some reverse ->
        let%arr reverse in
        Some (Sort_kind.reversible' ~reverse)
    in
    T.Leaf { leaf_header = header; initial_width; cell; visible; resizable; extra = sort }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let sorters t (local_ _graph) =
      let rec loop ~next_id ~acc = function
        | T.Leaf { extra = sort; _ } ->
          let acc = Map.set (acc : _ Int.Map.t) ~key:next_id ~data:sort in
          next_id + 1, acc
        | Group { children; _ } | Org_group children ->
          List.fold children ~init:(next_id, acc) ~f:(fun (next_id, acc) child ->
            loop ~next_id ~acc child)
      in
      let _, sorters = loop ~next_id:0 ~acc:Int.Map.empty t in
      sorters
      |> Map.to_alist
      |> List.map ~f:(fun (i, sorter) ->
        let%arr sorter in
        Option.map sorter ~f:(fun sorter -> i, sorter))
      |> Bonsai.all
      >>| Fn.compose Int.Map.of_alist_exn List.filter_opt
    ;;
  end

  let lift
    : type key data.
      (key, data) T.t list -> (key, data, Indexed_column_id.t) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data) T.t
      type nonrec key = key
      type nonrec data = data
      type column_id = int
      type column_id_cmp = Int.comparator_witness

      let instantiate_cells = T.instantiate_cells
      let headers = T.headers

      include W
    end
    in
    fun columns ->
      Column_intf.Y
        { value = T.Org_group columns; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_columns_with_sorter = struct
  module T = Dynamic_columns_shared (Extra_client_side)

  type ('key, 'data) t = ('key, 'data) T.t

  let column
    ?sort
    ?sort_reversed
    ?(initial_width = Columns.Column_structure.default_initial_width)
    ?(visible = true)
    ?(resizable = true)
    ~header
    ~cell
    ()
    =
    let sort =
      match sort, sort_reversed with
      | None, None -> None
      | Some forward, Some reverse -> Some { Sort_kind.forward; reverse }
      | Some forward, None -> Some (Sort_kind.reversible ~forward)
      | None, Some reverse -> Some (Sort_kind.reversible' ~reverse)
    in
    T.Leaf { leaf_header = header; initial_width; cell; visible; resizable; extra = sort }
  ;;

  let group ~label children = T.Group { group_header = label; children }
  let expand ~label child = group ~label [ child ]

  module W = struct
    let sorters t (local_ _graph) =
      let%arr t in
      let rec loop ~next_id ~acc = function
        | T.Leaf { extra = sort; _ } ->
          let acc = Map.set (acc : _ Int.Map.t) ~key:next_id ~data:sort in
          next_id + 1, acc
        | Group { children; _ } | Org_group children ->
          List.fold children ~init:(next_id, acc) ~f:(fun (next_id, acc) child ->
            loop ~next_id ~acc child)
      in
      let _, sorters = loop ~next_id:0 ~acc:Int.Map.empty t in
      sorters
      |> Map.to_alist
      |> List.filter_map ~f:(fun (i, sorter) ->
        Option.map sorter ~f:(fun sorter -> i, sorter))
      |> Int.Map.of_alist_exn
    ;;
  end

  let lift
    : type key data.
      (key, data) T.t list Bonsai.t
      -> (key, data, Indexed_column_id.t) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data) T.t Bonsai.t
      type nonrec key = key
      type nonrec data = data
      type column_id = int
      type column_id_cmp = Int.comparator_witness

      let headers = T.headers
      let instantiate_cells = T.instantiate_cells

      include W
    end
    in
    fun columns ->
      let value =
        let%arr columns in
        T.Org_group columns
      in
      Column_intf.Y { value; vtable = (module X); column_id = (module Int) }
  ;;

  module Sortable = Sortable
end

module Dynamic_experimental_with_sorter = struct
  module T = struct
    type ('key, 'data, 'column_id, 'column_id_cmp) t =
      { shared : ('key, 'data, 'column_id, 'column_id_cmp) Dynamic_experimental.T.t
      ; sorts :
          ('column_id Bonsai.t
           -> local_ Bonsai.graph
           -> ('key, 'data) Sort_kind.t option Bonsai.t)
            option
      }

    let headers ~wrap_header { shared; sorts = _ } =
      Dynamic_experimental.T.headers ~wrap_header shared
    ;;

    let instantiate_cells { shared; sorts = _ } =
      Dynamic_experimental.T.instantiate_cells shared
    ;;

    let sorters
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> local_ Bonsai.graph
        -> (column_id, (key, data) Sort_kind.t, column_id_cmp) Map.t Bonsai.t
      =
      fun { shared = { column_id; columns_as_a_set; _ }; sorts } graph ->
      let module Col_id = (val column_id) in
      match sorts with
      | None -> Bonsai.return (Map.empty (module Col_id))
      | Some sorts ->
        let sorts =
          Bonsai.assoc_set
            (module Col_id)
            columns_as_a_set
            ~f:(fun column -> sorts column)
            graph
        in
        Bonsai.Map.filter_map sorts ~f:Fn.id graph
    ;;
  end

  let build
    : type key data column_id column_id_cmp.
      ?sorts:
        (column_id Bonsai.t
         -> local_ Bonsai.graph
         -> (key, data) Sort_kind.t option Bonsai.t)
      -> (column_id, column_id_cmp) Comparator.Module.t
      -> columns:column_id list Bonsai.t
      -> render_header:(column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:
           (column_id Bonsai.t
            -> key Bonsai.t
            -> data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
      -> (key, data, column_id) Column_intf.with_sorter
    =
    let module X = struct
      type t = (key, data, column_id, column_id_cmp) T.t
      type nonrec key = key
      type nonrec data = data
      type nonrec column_id = column_id
      type nonrec column_id_cmp = column_id_cmp

      let headers = T.headers
      let sorters = T.sorters
      let instantiate_cells = T.instantiate_cells
    end
    in
    fun ?sorts column_id ~columns ~render_header ~render_cell ->
      let columns, columns_as_a_set = Dynamic_experimental.dedup columns column_id in
      let value =
        { T.shared = { column_id; columns; columns_as_a_set; render_header; render_cell }
        ; sorts
        }
      in
      Column_intf.Y { value; vtable = (module X); column_id }
  ;;

  module Sortable = Sortable
  module Sort_kind = Sort_kind
end
