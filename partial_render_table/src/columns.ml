open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Sort_kind = Column_intf.Sort_kind

module Column_structure = struct
  module Static = struct
    type 'column_id t =
      | Leaf of 'column_id
      | Group of
          { children : 'column_id t list
          ; group_header : Vdom.Node.t Bonsai.t
          }

    let leaf col_id = Leaf col_id
    let group ~label children = Group { children; group_header = label }
  end

  module Dynamic = struct
    type 'column_id t =
      | Leaf of 'column_id
      | Group of
          { children : 'column_id t list
          ; group_header : Vdom.Node.t
          }

    let leaf col_id = Leaf col_id
    let group ~label children = Group { children; group_header = label }
  end

  module Static_or_dynamic = struct
    type 'column_id t =
      | Static of 'column_id Static.t list
      | Dynamic of 'column_id Dynamic.t list Bonsai.t
  end

  type 'column_id t =
    { cols : 'column_id Static_or_dynamic.t
    ; initial_widths : ('column_id -> Css_gen.Length.t) Bonsai.t option
    ; resizable : ('column_id -> bool) Bonsai.t option
    }

  module Group = struct
    include Static

    let lift ts =
      { cols = Static_or_dynamic.Static ts; initial_widths = None; resizable = None }
    ;;
  end

  module Group_dynamic = struct
    include Dynamic

    let lift ts =
      { cols = Static_or_dynamic.Dynamic ts; initial_widths = None; resizable = None }
    ;;
  end

  let flat col_ids = List.map col_ids ~f:(fun x -> Static.leaf x) |> Group.lift

  let flat_dynamic col_ids =
    let cols =
      let%arr col_ids in
      List.map col_ids ~f:(fun x -> Dynamic.Leaf x)
    in
    Group_dynamic.lift cols
  ;;

  let with_initial_widths t ~f = { t with initial_widths = Some f }
  let with_is_resizable t ~f = { t with resizable = Some f }
  let default_initial_width = `Px 50
end

module Render_cell = struct
  type ('key, 'data, 'column_id) t =
    | Pure of ('column_id -> 'key -> 'data -> Vdom.Node.t) Bonsai.t
    | Stateful_rows of
        ('key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> ('column_id -> Vdom.Node.t) Bonsai.t)
    | Stateful_cells of
        ('column_id Bonsai.t
         -> 'key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> Vdom.Node.t Bonsai.t)
end

module Expert = struct
  module Columns = struct
    (* The ordered set of column Ids is derived from the structure, but we need it for
       both [headers] and [instantiate_cells], so we factor it out. *)
    type ('column_id, 'column_id_cmp) t =
      | Static of ('column_id Column_structure.Static.t list * 'column_id list)
      | Dynamic of
          ('column_id Column_structure.Dynamic.t list Bonsai.t
          * 'column_id list Bonsai.t
          * ('column_id, unit, 'column_id_cmp) Map.t Bonsai.t)
  end

  type ('key, 'data, 'column_id, 'column_id_cmp) t =
    { column_id : ('column_id, 'column_id_cmp) Comparator.Module.t
    ; columns : ('column_id, 'column_id_cmp) Columns.t
    ; initial_widths : ('column_id -> Css_gen.Length.t) Bonsai.t
    ; resizable : ('column_id -> bool) Bonsai.t
    ; render_cell : ('key, 'data, 'column_id) Render_cell.t
    ; render_header : 'column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
    }

  let create
    : type key data column column_cmp.
      (column, column_cmp) Comparator.Module.t
      -> column Column_structure.t
      -> render_cell:(key, data, column) Render_cell.t
      -> render_header:(column Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> (key, data, column, column_cmp) t
    =
    fun column_id
      { Column_structure.cols; initial_widths; resizable }
      ~render_cell
      ~render_header ->
    let module Col_id = (val column_id) in
    let columns =
      match cols with
      | Static cols ->
        let rec collect_col_ids acc = function
          | Column_structure.Static.Leaf column_id -> column_id :: acc
          | Group { children; _ } -> List.fold children ~init:acc ~f:collect_col_ids
        in
        let all_col_ids = List.fold cols ~init:[] ~f:collect_col_ids |> List.rev in
        Columns.Static (cols, all_col_ids)
      | Dynamic cols ->
        let rec collect_col_ids acc = function
          | Column_structure.Dynamic.Leaf column_id -> column_id :: acc
          | Group { children; _ } -> List.fold children ~init:acc ~f:collect_col_ids
        in
        let all_col_ids =
          let%arr cols in
          List.fold cols ~init:[] ~f:collect_col_ids |> List.rev
        in
        let col_ids_map =
          let%arr all_col_ids in
          List.map all_col_ids ~f:(fun id -> id, ())
          |> Map.of_alist_reduce (module Col_id) ~f:(fun () () -> ())
        in
        Columns.Dynamic (cols, all_col_ids, col_ids_map)
    in
    let initial_widths =
      Option.value
        initial_widths
        ~default:(return (fun _ -> Column_structure.default_initial_width))
    in
    let resizable = Option.value resizable ~default:(return (fun _ -> true)) in
    { column_id; columns; render_cell; render_header; initial_widths; resizable }
  ;;

  let headers
    : type key data column_id column_id_cmp.
      wrap_header:(column_id:column_id -> Vdom.Node.t -> Vdom.Node.t) Bonsai.t
      -> (key, data, column_id, column_id_cmp) t
      -> local_ Bonsai.graph
      -> column_id Header_tree.t Bonsai.t
    =
    fun ~wrap_header
      { columns; column_id; initial_widths; resizable; render_header; _ }
      (local_ graph) ->
    match columns with
    | Static (cols, _) ->
      let rec loop_static = function
        | Column_structure.Static.Leaf column_id ->
          let%arr header = render_header (Bonsai.return column_id) graph
          and initial_widths
          and resizable
          and wrap_header in
          Header_tree.leaf
            ~header:(wrap_header ~column_id header)
            ~visible:true
            ~initial_width:(initial_widths column_id)
            ~column_id
            ~resizable:(resizable column_id)
        | Group { children; group_header } ->
          let tree =
            let%arr header = group_header
            and children = Bonsai.all (List.map ~f:loop_static children) in
            Header_tree.group ~header children
          in
          tree
      in
      let%arr children = Bonsai.all (List.map ~f:loop_static cols) in
      Header_tree.org_group children
    | Dynamic (cols, _, col_ids_map) ->
      let rec loop ~header_map ~initial_widths ~resizable = function
        | Column_structure.Dynamic.Leaf column_id ->
          (* Should always exist, because we just traversed this same tree to get all the
             column ids. *)
          let header =
            Map.find header_map column_id
            |> Option.value
                 ~default:(Vdom.Node.text "BUG: could not find header for column")
          in
          let resizable = resizable column_id in
          let initial_width = initial_widths column_id in
          Header_tree.leaf ~header ~visible:true ~initial_width ~column_id ~resizable
        | Group { children; group_header } ->
          let children =
            List.map ~f:(loop ~header_map ~initial_widths ~resizable) children
          in
          Header_tree.group ~header:group_header children
      in
      let render_header column_id (_ : unit Bonsai.t) (local_ graph) =
        let%arr header = render_header column_id graph
        and wrap_header
        and column_id in
        wrap_header ~column_id header
      in
      let%arr cols
      and header_map = Bonsai.assoc column_id col_ids_map ~f:render_header graph
      and initial_widths
      and resizable in
      Header_tree.org_group
        (List.map ~f:(loop ~header_map ~initial_widths ~resizable) cols)
  ;;

  let instantiate_cells
    : type key data column_id column_id_cmp.
      (key, data, column_id, column_id_cmp) t
      -> (key, _) Comparator.Module.t
      -> (key * data) Opaque_map.t Bonsai.t
      -> local_ Bonsai.graph
      -> (key * (column_id * Vdom.Node.t) list) Opaque_map.t Bonsai.t
    =
    fun { column_id; columns; render_cell; _ } key_cmp rows ->
    let module Col_id = (val column_id) in
    match columns with
    | Dynamic (_, all_column_ids, col_ids_map) ->
      let assoc_on_rows ~f =
        Bonsai.Expert.assoc_on
          (module Opaque_map.Key)
          key_cmp
          rows
          ~get_model_key:(fun _ (k, _) -> k)
          ~f:(fun _opaque_key data graph ->
            let%sub key, data = data in
            let visible_cells = f key data graph in
            let cell_list =
              let%arr visible_cells and all_column_ids in
              List.map all_column_ids ~f:(fun col ->
                let cells =
                  Map.find visible_cells col
                  |> Option.value
                       ~default:(Vdom.Node.none_deprecated [@alert "-deprecated"])
                in
                col, cells)
            in
            Bonsai.both key cell_list)
      in
      (match render_cell with
       | Stateful_cells render_cell ->
         assoc_on_rows ~f:(fun key data (local_ graph) ->
           Bonsai.assoc
             (module Col_id)
             col_ids_map
             ~f:(fun col_id _ (local_ graph) -> render_cell col_id key data graph)
             graph)
       | Stateful_rows render_cell ->
         assoc_on_rows ~f:(fun key data (local_ graph) ->
           let render_cell = render_cell key data graph in
           let%arr col_ids_map and render_cell in
           Map.mapi col_ids_map ~f:(fun ~key:col_id ~data:() -> render_cell col_id))
       | Pure render_cell ->
         Bonsai.Incr.compute
           (Bonsai.both (Bonsai.both all_column_ids render_cell) rows)
           ~f:(fun all ->
             let%pattern_bind.Ui_incr ids_and_render, rows = all in
             let%bind.Ui_incr ids_and_render in
             let all_column_ids, render_cell = ids_and_render in
             Ui_incr.Map.map rows ~f:(fun (key, data) ->
               let cells =
                 List.map all_column_ids ~f:(fun col_id ->
                   col_id, render_cell col_id key data)
               in
               key, cells)))
    | Static (_, all_column_ids) ->
      let assoc_on_rows ~make_render_cell ~apply_render_cell =
        Bonsai.Expert.assoc_on
          (module Opaque_map.Key)
          key_cmp
          rows
          ~get_model_key:(fun _ (k, _) -> k)
          ~f:(fun _opaque_key data graph ->
            let%sub key, data = data in
            let render_cell = make_render_cell key data graph in
            let cells =
              List.fold_map
                all_column_ids
                ~init:(Map.empty (module Col_id))
                ~f:(fun seen col ->
                  match Map.find seen col with
                  | Some r -> seen, r
                  | None ->
                    let cell = apply_render_cell render_cell key col data graph in
                    (* This [add_exn] is safe, because we just checked that the col has
                       not been seen. *)
                    Map.add_exn seen ~key:col ~data:cell, cell)
              |> snd
              |> Bonsai.all
            in
            Bonsai.both key cells)
      in
      (match render_cell with
       | Stateful_cells render_cell ->
         assoc_on_rows
           ~make_render_cell:(fun _ _ _ -> ())
           ~apply_render_cell:(fun () key col data graph ->
             let%arr cell = render_cell (return col) key data graph in
             col, cell)
       | Stateful_rows render_cell ->
         assoc_on_rows
           ~make_render_cell:(fun key data graph -> render_cell key data graph)
           ~apply_render_cell:(fun render_cell _ col _ _ ->
             let%arr render_cell in
             col, render_cell col)
       | Pure render_cell ->
         Bonsai.Incr.compute (Bonsai.both render_cell rows) ~f:(fun all ->
           let%pattern_bind.Ui_incr render_cell, rows = all in
           let%bind.Ui_incr render_cell in
           Ui_incr.Map.map rows ~f:(fun (key, data) ->
             let cells =
               List.map all_column_ids ~f:(fun col_id ->
                 col_id, render_cell col_id key data)
             in
             key, cells)))
  ;;

  let build
    : type key data column column_cmp.
      (column, column_cmp) Comparator.Module.t
      -> columns:column Column_structure.t
      -> render_header:(column Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:(key, data, column) Render_cell.t
      -> (key, data, column) Column_intf.t
    =
    let module X = struct
      type t' = (key, data, column, column_cmp) t
      type t = t'
      type nonrec key = key
      type nonrec data = data
      type column_id = column

      let headers = headers
      let instantiate_cells = instantiate_cells
    end
    in
    fun column_id ~columns ~render_header ~render_cell ->
      let value = create column_id columns ~render_cell ~render_header in
      Column_intf.T { value; vtable = (module X); column_id }
  ;;

  module Sortable = Sortable
end

module Basic = struct
  module T = struct
    type ('key, 'data, 'column_id, 'column_id_cmp) t =
      { from_expert : ('key, 'data, 'column_id, 'column_id_cmp) Expert.t
      ; sorts :
          ('column_id Bonsai.t
           -> local_ Bonsai.graph
           -> ('key, 'data) Sort_kind.t option Bonsai.t)
            option
      }

    let instantiate_cells { from_expert; _ } key_cmp rows =
      Expert.instantiate_cells from_expert key_cmp rows
    ;;

    let headers ~wrap_header { from_expert; _ } = Expert.headers ~wrap_header from_expert

    let sorters
      : type key data column_id column_id_cmp.
        (key, data, column_id, column_id_cmp) t
        -> local_ Bonsai.graph
        -> (column_id, (key, data) Sort_kind.t, column_id_cmp) Map.t Bonsai.t
      =
      fun { sorts; from_expert } (local_ graph) ->
      let module Col_id = (val from_expert.column_id) in
      ((match sorts, from_expert.columns with
        | None, _ -> return (Map.empty (module Col_id))
        | Some sorts, Static (_, columns) ->
          List.fold
            ~init:(Map.empty (module Col_id))
            columns
            ~f:(fun acc col_id ->
              match Map.add acc ~key:col_id ~data:(sorts (return col_id)) with
              | `Ok map -> map
              | _ ->
                (* If there's a duplicate column, we've already called [sorts] with the
                   same input, and should have gotten the same output, so we are done. *)
                acc)
          |> fun x -> Bonsai.all_map x graph
        | Some sorts, Dynamic (_, _, columns_map) ->
          Bonsai.assoc
            (module Col_id)
            columns_map
            ~f:(fun column (_ : unit Bonsai.t) -> sorts column)
            graph)
       |> fun x -> Bonsai.Map.filter_map x ~f:Fn.id)
        graph
    ;;
  end

  let build
    : type key data column_id column_id_cmp.
      ?sorts:
        (column_id Bonsai.t
         -> local_ Bonsai.graph
         -> (key, data) Sort_kind.t option Bonsai.t)
      -> (column_id, column_id_cmp) Comparator.Module.t
      -> columns:column_id Column_structure.t
      -> render_header:(column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:(key, data, column_id) Render_cell.t
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
      let from_expert = Expert.create column_id columns ~render_cell ~render_header in
      let value = { T.sorts; from_expert } in
      Column_intf.Y { value; vtable = (module X); column_id }
  ;;

  module Sortable = Sortable
end
