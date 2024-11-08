open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Prt = Bonsai_web_ui_partial_render_table
module Table = Prt.Expert
open Bonsai_perf_shared

module Row = struct
  module T = struct
    type t =
      { symbol : string
      ; edge : float
      ; max_edge : float
      ; bsize : int
      ; bid : float
      ; ask : float
      ; asize : int
      }
    [@@deriving compare, fields ~fields, sexp, typed_fields, quickcheck]
  end

  include T
  include Comparator.Make (T)

  let of_int i =
    { symbol = [%string "JANE%{i#Int}"]
    ; edge = Float.of_int i
    ; max_edge = Float.of_int i
    ; bsize = i
    ; bid = Float.of_int i
    ; ask = Float.of_int i
    ; asize = i
    }
  ;;

  let init_rows n = List.init n ~f:(fun x -> x, of_int x) |> Map.of_alist_exn (module Int)

  let random () : t =
    let fix_digits x = Float.round (x *. 100.) /. 100. in
    let symbol =
      let rchar () = Char.to_int 'A' + Random.int 26 |> Char.of_int_exn in
      String.init 4 ~f:(fun (_ : int) -> rchar ())
    in
    let edge = fix_digits (Float.of_int (Random.int 10) /. 100.) in
    let max_edge = fix_digits (edge +. (Float.of_int (Random.int 10) /. 100.)) in
    let fair = 10. +. (Float.of_int (Random.int 10000) /. 100.) in
    let bsize = (1 + Random.int 20) * 100 in
    let asize = Int.max 100 (bsize + (100 * (Random.int 5 - 2))) in
    let bid = fix_digits (fair -. (Float.of_int (Random.int 20) /. 100.)) in
    let ask = fix_digits (fair +. (Float.of_int (Random.int 20) /. 100.)) in
    { symbol; edge; max_edge; bsize; asize; bid; ask }
  ;;

  let many_random n =
    List.init n ~f:(fun i -> i, random ()) |> Map.of_alist_exn (module Int)
  ;;
end

module type S = sig
  type column_id

  val first_column : column_id
  val all : local_ Bonsai.graph -> (int, Row.t, column_id) Table.Columns.t
  val with_column_groups : local_ Bonsai.graph -> (int, Row.t, column_id) Table.Columns.t
end

let return_opaque_to_constant_fold x = Bonsai.Expert.Var.(create x |> value)

let incr_button value incr =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> incr) ]
    [ Vdom.Node.text (Int.to_string value) ]
;;

let counter (local_ graph) =
  let value, set_value = Bonsai.state 0 graph in
  let%arr value and set_value in
  incr_button value (set_value (value + 1))
;;

let dynamic_cells ~counters_in_cells ~duplicate_col : (module S) =
  (module struct
    module type S = sig
      type t [@@deriving compare]

      val to_string : t -> string
    end

    module Column = Table.Columns.Dynamic_cells

    type column_id = Prt.Indexed_column_id.t

    let first_column = Prt.Indexed_column_id.of_int 0

    let column_helper
      (type a)
      (module M : S with type t = a)
      ?visible
      (field : (_, a) Field.t)
      =
      Column.column
        ?visible
        ~header:(return (Vdom.Node.text (Fieldslib.Field.name field)))
        ~cell:(fun ~key:_ ~data graph ->
          let%sub counter =
            match counters_in_cells with
            | true -> counter graph
            | false -> return (Vdom.Node.none_deprecated [@alert "-deprecated"])
          in
          let%arr data and counter in
          Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)); counter ])
        ()
    ;;

    let all _ =
      ([ column_helper (module String) Row.Fields.symbol
       ; column_helper (module Float) Row.Fields.edge
       ; column_helper (module Float) Row.Fields.max_edge
       ; column_helper (module Int) Row.Fields.bsize
       ; column_helper (module Float) Row.Fields.bid
       ; column_helper (module Float) Row.Fields.ask
       ; column_helper (module Int) Row.Fields.asize
       ]
       @ if duplicate_col then [ column_helper (module Float) Row.Fields.edge ] else [])
      |> Column.lift
    ;;

    let with_column_groups _ =
      ([ column_helper (module String) Row.Fields.symbol
       ; Column.group
           ~label:(Vdom.Node.text "Edge" |> return_opaque_to_constant_fold)
           [ column_helper (module Float) Row.Fields.edge
           ; column_helper
               (module Float)
               Row.Fields.max_edge
               ~visible:(return_opaque_to_constant_fold false)
           ]
       ; Column.group
           ~label:(Vdom.Node.text "Book" |> return_opaque_to_constant_fold)
           [ column_helper (module Int) Row.Fields.bsize
           ; column_helper (module Float) Row.Fields.bid
           ; column_helper (module Float) Row.Fields.ask
           ; column_helper (module Int) Row.Fields.asize
           ]
       ]
       @ if duplicate_col then [ column_helper (module Float) Row.Fields.edge ] else [])
      |> Column.lift
    ;;
  end)
;;

module Which_dynamic_cols = struct
  type t =
    | Counters
    | No_counters
    | No_counters_constant_foldable
  [@@deriving equal, compare, sexp_of, enumerate, hash]
end

module Map_key = struct
  module T = struct
    type t = string * int [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

let dynamic_cols ~which_dynamic_cols ~duplicate_col : (module S) =
  (module struct
    module type S = sig
      type t [@@deriving compare]

      val to_string : t -> string
    end

    module Column = Table.Columns.Dynamic_columns

    type column_id = Prt.Indexed_column_id.t

    let first_column = Prt.Indexed_column_id.of_int 0

    let column_helper
      (type a)
      (module M : S with type t = a)
      ?build_button
      (field : (_, a) Field.t)
      =
      let name = Fieldslib.Field.name field in
      Column.column
        ~header:(Vdom.Node.text name)
        ~cell:(fun ~key ~data ->
          Vdom.Node.div
            [ Vdom.Node.text (M.to_string (Field.get field data))
            ; (match build_button with
               | None -> Vdom.Node.none_deprecated [@alert "-deprecated"]
               | Some build_button -> build_button name key)
            ])
        ()
    ;;

    let build_cols cols (local_ graph) =
      (match which_dynamic_cols with
       | Which_dynamic_cols.No_counters ->
         cols ~build_button:None |> return_opaque_to_constant_fold
       | No_counters_constant_foldable -> cols ~build_button:None |> return
       | Counters ->
         let state, incr_cell =
           Bonsai.state_machine0
             ~default_model:(Map.empty (module Map_key))
             ~apply_action:(fun _ctx map key ->
               Map.update map key ~f:(function
                 | None -> 1
                 | Some x -> x + 1))
             graph
         in
         let%arr state and incr_cell in
         let build_button column_id row_id =
           let state = Map.find state (column_id, row_id) |> Option.value ~default:0 in
           let incr = incr_cell (column_id, row_id) in
           incr_button state incr
         in
         cols ~build_button:(Some build_button))
      |> Column.lift
    ;;

    let all (local_ graph) =
      let cols ~build_button =
        [ column_helper (module String) Row.Fields.symbol ?build_button
        ; column_helper (module Float) Row.Fields.edge ?build_button
        ; column_helper (module Float) Row.Fields.max_edge ?build_button
        ; column_helper (module Int) Row.Fields.bsize ?build_button
        ; column_helper (module Float) Row.Fields.bid ?build_button
        ; column_helper (module Float) Row.Fields.ask ?build_button
        ; column_helper (module Int) Row.Fields.asize ?build_button
        ]
        @
        if duplicate_col
        then [ column_helper (module Float) Row.Fields.edge ?build_button ]
        else []
      in
      build_cols cols graph
    ;;

    let with_column_groups (local_ graph) =
      let cols ~build_button =
        [ column_helper (module String) Row.Fields.symbol ?build_button
        ; Column.group
            ~label:(Vdom.Node.text "Edge")
            [ column_helper (module Float) Row.Fields.edge ?build_button ]
        ; Column.group
            ~label:(Vdom.Node.text "Book")
            [ column_helper (module Int) Row.Fields.bsize ?build_button
            ; column_helper (module Float) Row.Fields.bid ?build_button
            ; column_helper (module Float) Row.Fields.ask ?build_button
            ; column_helper (module Int) Row.Fields.asize ?build_button
            ]
        ]
        @
        if duplicate_col
        then [ column_helper (module Float) Row.Fields.edge ?build_button ]
        else []
      in
      build_cols cols graph
    ;;
  end)
;;

let dynamic_experimental ~counters_in_cells ~constant_foldable_cols : (module S) =
  (module struct
    module Column = Table.Columns.Dynamic_experimental

    module Col_id = struct
      include Row.Typed_field.Packed
      include Comparator.Make (Row.Typed_field.Packed)
    end

    type column_id = Col_id.t

    let first_column = Row.Typed_field.Packed.all |> List.hd_exn

    let render_header col _ =
      let%arr { Row.Typed_field.Packed.f = T field } = col in
      Vdom.Node.text (Row.Typed_field.name field)
    ;;

    let render_cell col _k row (local_ graph) =
      let counter =
        match counters_in_cells with
        | true -> counter graph
        | false -> return (Vdom.Node.none_deprecated [@alert "-deprecated"])
      in
      let%arr { Row.Typed_field.Packed.f = T field } = col
      and row
      and counter in
      let float, int = Float.to_string, Int.to_string in
      let value = Row.Typed_field.get field row in
      let s =
        match field with
        | Symbol -> (value : string)
        | Edge -> float value
        | Max_edge -> float value
        | Bsize -> int value
        | Bid -> float value
        | Ask -> float value
        | Asize -> int value
      in
      Vdom.Node.div [ Vdom.Node.text s; counter ]
    ;;

    let all _ : (int, Row.t, column_id) Table.Columns.t =
      Column.build
        (module Col_id)
        ~render_header
        ~render_cell
        ~columns:
          ((if constant_foldable_cols
            then fun x -> return x
            else return_opaque_to_constant_fold)
             Row.Typed_field.Packed.all)
    ;;

    let with_column_groups = all
  end)
;;

module New_api_cols = struct
  type t =
    | Static
    | Dynamic
    | Dynamic_constant_foldable
  [@@deriving equal, compare, sexp_of, enumerate, hash]
end

module Render_cell_kind = struct
  type t =
    | Pure
    | Stateful_rows
    | Stateful_cells
  [@@deriving equal, compare, sexp_of, enumerate, hash, string]
end

let new_api ~counters_in_cells ~cols ~render_cell_kind ~duplicate_col : (module S) =
  (module struct
    module Col_id = struct
      include Row.Typed_field.Packed
      include Comparator.Make (Row.Typed_field.Packed)
    end

    type column_id = Col_id.t

    let first_column = Row.Typed_field.Packed.all |> List.hd_exn

    let render_header col (local_ _graph) =
      let%arr { Row.Typed_field.Packed.f = T field } = col in
      Vdom.Node.text (Row.Typed_field.name field)
    ;;

    let pure (local_ graph) =
      let build_button =
        match counters_in_cells with
        | true ->
          let state, incr_cell =
            Bonsai.state_machine0
              ~default_model:(Map.empty (module Map_key))
              ~apply_action:(fun _ctx map key ->
                Map.update map key ~f:(function
                  | None -> 1
                  | Some x -> x + 1))
              graph
          in
          let%arr state and incr_cell in
          fun column_id row_id ->
            let state = Map.find state (column_id, row_id) |> Option.value ~default:0 in
            let incr = incr_cell (column_id, row_id) in
            incr_button state incr
        | false ->
          Bonsai.return (fun _ _ -> Vdom.Node.none_deprecated [@alert "-deprecated"])
      in
      let%arr build_button in
      fun { Row.Typed_field.Packed.f = T field } k row ->
        let value = Row.Typed_field.get field row in
        let float, int = Float.to_string, Int.to_string in
        let s =
          match field with
          | Symbol -> (value : string)
          | Edge -> float value
          | Max_edge -> float value
          | Bsize -> int value
          | Bid -> float value
          | Ask -> float value
          | Asize -> int value
        in
        Vdom.Node.div [ Vdom.Node.text s; build_button (Row.Typed_field.name field) k ]
    ;;

    let stateful_rows _k row (local_ graph) =
      let build_button =
        match counters_in_cells with
        | true ->
          let state, incr_cell =
            Bonsai.state_machine0
              ~default_model:(Map.empty (module String))
              ~apply_action:(fun _ctx map key ->
                Map.update map key ~f:(function
                  | None -> 1
                  | Some x -> x + 1))
              graph
          in
          let%arr state and incr_cell in
          fun column_id ->
            let state = Map.find state column_id |> Option.value ~default:0 in
            let incr = incr_cell column_id in
            incr_button state incr
        | false ->
          Bonsai.return (fun _ -> Vdom.Node.none_deprecated [@alert "-deprecated"])
      in
      let%arr row and build_button in
      fun { Row.Typed_field.Packed.f = T field } ->
        let value = Row.Typed_field.get field row in
        let float, int = Float.to_string, Int.to_string in
        let s =
          match field with
          | Symbol -> (value : string)
          | Edge -> float value
          | Max_edge -> float value
          | Bsize -> int value
          | Bid -> float value
          | Ask -> float value
          | Asize -> int value
        in
        Vdom.Node.div [ Vdom.Node.text s; build_button (Row.Typed_field.name field) ]
    ;;

    let stateful_cells col _k row (local_ graph) =
      let counter =
        match counters_in_cells with
        | true -> counter graph
        | false -> return (Vdom.Node.none_deprecated [@alert "-deprecated"])
      in
      let%arr { Row.Typed_field.Packed.f = T field } = col
      and row
      and counter in
      let float, int = Float.to_string, Int.to_string in
      let value = Row.Typed_field.get field row in
      let s =
        match field with
        | Symbol -> (value : string)
        | Edge -> float value
        | Max_edge -> float value
        | Bsize -> int value
        | Bid -> float value
        | Ask -> float value
        | Asize -> int value
      in
      Vdom.Node.div [ Vdom.Node.text s; counter ]
    ;;

    let render_cell graph =
      match render_cell_kind with
      | Render_cell_kind.Pure -> Prt.Render_cell.Pure (pure graph)
      | Stateful_rows -> Stateful_rows stateful_rows
      | Stateful_cells -> Stateful_cells stateful_cells
    ;;

    let all graph : (int, Row.t, column_id) Table.Columns.t =
      let structure =
        let extra = if duplicate_col then [ Row.Typed_field.Packed.pack Edge ] else [] in
        match cols with
        | New_api_cols.Dynamic ->
          Prt.Column_structure.flat_dynamic
            (return_opaque_to_constant_fold (Row.Typed_field.Packed.all @ extra))
        | Dynamic_constant_foldable ->
          Prt.Column_structure.flat_dynamic (return (Row.Typed_field.Packed.all @ extra))
        | Static -> Prt.Column_structure.flat (Row.Typed_field.Packed.all @ extra)
      in
      Table.New_columns.build
        (module Col_id)
        ~render_header
        ~render_cell:(render_cell graph)
        ~columns:structure
    ;;

    let with_column_groups (local_ graph) =
      let structure =
        let pack = Row.Typed_field.Packed.pack in
        match cols with
        | New_api_cols.Dynamic ->
          Prt.Column_structure.Group_dynamic.(
            lift
              (return_opaque_to_constant_fold
                 ([ leaf (pack Symbol)
                  ; group ~label:(Vdom.Node.text "Edge") [ leaf (pack Edge) ]
                  ; group
                      ~label:(Vdom.Node.text "Book")
                      [ leaf (pack Bsize)
                      ; leaf (pack Bid)
                      ; leaf (pack Ask)
                      ; leaf (pack Asize)
                      ]
                  ]
                  @ if duplicate_col then [ leaf (pack Edge) ] else [])))
        | Dynamic_constant_foldable ->
          Prt.Column_structure.Group_dynamic.(
            lift
              (return
                 ([ leaf (pack Symbol)
                  ; group ~label:(Vdom.Node.text "Edge") [ leaf (pack Edge) ]
                  ; group
                      ~label:(Vdom.Node.text "Book")
                      [ leaf (pack Bsize)
                      ; leaf (pack Bid)
                      ; leaf (pack Ask)
                      ; leaf (pack Asize)
                      ]
                  ]
                  @ if duplicate_col then [ leaf (pack Edge) ] else [])))
        | Static ->
          Prt.Column_structure.Group.(
            lift
              ([ leaf (pack Symbol)
               ; group
                   ~label:(return_opaque_to_constant_fold (Vdom.Node.text "Edge"))
                   [ leaf (pack Edge) ]
               ; group
                   ~label:(return_opaque_to_constant_fold (Vdom.Node.text "Book"))
                   [ leaf (pack Bsize)
                   ; leaf (pack Bid)
                   ; leaf (pack Ask)
                   ; leaf (pack Asize)
                   ]
               ]
               @ if duplicate_col then [ leaf (pack Edge) ] else []))
      in
      Table.New_columns.build
        (module Col_id)
        ~render_header
        ~render_cell:(render_cell graph)
        ~columns:structure
    ;;
  end)
;;

module Prt_input = struct
  type ('key, 'data, 'cmp) t =
    { filter : (key:'key -> data:'data -> bool) option
    ; order : ('key, 'data, 'cmp) Incr_map_collate.Compare.t
    ; rank_range : int Incr_map_collate.Collate.Which_range.t
    ; key_range : 'key Incr_map_collate.Collate.Which_range.t
    ; resize_column_widths_to_fit : bool
    ; row_height : [ `Px of int ]
    ; map : ('key, 'data, 'cmp) Map.t
    }

  let create
    ?(filter = None)
    ?(order = Incr_map_collate.Compare.Unchanged)
    ?(rank_range = Incr_map_collate.Collate.Which_range.To 100)
    ?(key_range = Incr_map_collate.Collate.Which_range.All_rows)
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
        (Incr_map_collate.Collate.Which_range.Between (i, i + window_size - 1)))
    |> Interaction.many_with_stabilizations
  ;;
end

module Action = struct
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

module Prt_output = struct
  type t =
    { view : Vdom.Node.t
    ; range : int * int
    ; inject : int Action.t -> unit Effect.t
    }
end

module Config = struct
  module Render_cell_kind = Render_cell_kind
  module New_api_cols = New_api_cols
  module Which_dynamic_cols = Which_dynamic_cols

  type input = (int, Row.t, Int.comparator_witness) Prt_input.t
  type output = Prt_output.t
  type action = int Action.t

  module New_api_params = struct
    type t =
      { counters_in_cells : bool
      ; render_cell_kind : Render_cell_kind.t
      ; cols : New_api_cols.t
      ; col_groups : bool
      ; duplicate_col : bool
      }
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module Dynamic_cells_params = struct
    type t =
      { counters_in_cells : bool
      ; col_groups : bool
      ; duplicate_col : bool
      }
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module Dynamic_cols_params = struct
    type t =
      { which_dynamic_cols : Which_dynamic_cols.t
      ; col_groups : bool
      ; duplicate_col : bool
      }
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  module Dynamic_experimental_params = struct
    type t =
      { counters_in_cells : bool
      ; constant_foldable_cols : bool
      }
    [@@deriving equal, compare, sexp_of, enumerate, hash]
  end

  type t =
    | New_api of New_api_params.t
    | Dynamic_cells of Dynamic_cells_params.t
    | Dynamic_cols of Dynamic_cols_params.t
    | Dynamic_experimental of Dynamic_experimental_params.t
  [@@deriving equal, compare, sexp_of, enumerate, hash]

  let name =
    let counters = " (counters)" in
    let constant_foldable = " (cf cols)" in
    let groups = " (groups)" in
    let flat = " (flat)" in
    function
    | New_api { counters_in_cells; col_groups; cols; render_cell_kind; duplicate_col = _ }
      ->
      "new"
      ^ (match render_cell_kind with
         | Pure -> " pure"
         | Stateful_rows -> " incr_rows"
         | Stateful_cells -> " incr_cells")
      ^ (match cols with
         | New_api_cols.Static -> " (static cols)"
         | Dynamic -> " (dynamic cols)"
         | Dynamic_constant_foldable -> constant_foldable)
      ^ (if counters_in_cells then counters else "")
      ^ if col_groups then groups else flat
    | Dynamic_cells { counters_in_cells; col_groups; duplicate_col = _ } ->
      "dyn cells"
      ^ (if counters_in_cells then counters else "")
      ^ if col_groups then groups else flat
    | Dynamic_cols { col_groups; which_dynamic_cols; duplicate_col = _ } ->
      "dyn cols"
      ^ (match which_dynamic_cols with
         | Which_dynamic_cols.Counters -> counters
         | No_counters_constant_foldable -> constant_foldable
         | No_counters -> "")
      ^ if col_groups then groups else flat
    | Dynamic_experimental { counters_in_cells; constant_foldable_cols } ->
      "dyn-exp"
      ^ (if counters_in_cells then counters else "")
      ^ if constant_foldable_cols then constant_foldable else ""
  ;;

  let full_power_comparison =
    [ New_api
        { col_groups = true
        ; cols = Dynamic
        ; counters_in_cells = true
        ; render_cell_kind = Stateful_cells
        ; duplicate_col = false
        }
    ; New_api
        { col_groups = true
        ; cols = Dynamic
        ; counters_in_cells = true
        ; render_cell_kind = Stateful_rows
        ; duplicate_col = false
        }
    ; New_api
        { col_groups = true
        ; cols = Dynamic
        ; counters_in_cells = true
        ; render_cell_kind = Pure
        ; duplicate_col = false
        }
    ; New_api
        { col_groups = true
        ; cols = Static
        ; counters_in_cells = true
        ; render_cell_kind = Stateful_cells
        ; duplicate_col = false
        }
    ; New_api
        { col_groups = true
        ; cols = Static
        ; counters_in_cells = true
        ; render_cell_kind = Stateful_rows
        ; duplicate_col = false
        }
    ; New_api
        { col_groups = true
        ; cols = Static
        ; counters_in_cells = true
        ; render_cell_kind = Pure
        ; duplicate_col = false
        }
    ; Dynamic_cols
        { which_dynamic_cols = Counters; col_groups = true; duplicate_col = false }
    ; Dynamic_cells { counters_in_cells = true; col_groups = true; duplicate_col = false }
    ]
  ;;

  module For_component = struct
    type t =
      | T :
          { columns : local_ Bonsai.graph -> (int, Row.t, 'column_id) Table.Columns.t
          ; first_column : 'column_id
          }
          -> t

    let build = function
      | New_api { counters_in_cells; cols; col_groups; render_cell_kind; duplicate_col }
        ->
        let module Table =
          (val new_api ~counters_in_cells ~cols ~render_cell_kind ~duplicate_col)
        in
        T
          { first_column = Table.first_column
          ; columns = (if col_groups then Table.with_column_groups else Table.all)
          }
      | Dynamic_experimental { counters_in_cells; constant_foldable_cols } ->
        let module Table =
          (val dynamic_experimental ~counters_in_cells ~constant_foldable_cols)
        in
        T { first_column = Table.first_column; columns = Table.all }
      | Dynamic_cols { col_groups; which_dynamic_cols; duplicate_col } ->
        let module Table = (val dynamic_cols ~which_dynamic_cols ~duplicate_col) in
        T
          { first_column = Table.first_column
          ; columns = (if col_groups then Table.with_column_groups else Table.all)
          }
      | Dynamic_cells { counters_in_cells; col_groups; duplicate_col } ->
        let module Table = (val dynamic_cells ~counters_in_cells ~duplicate_col) in
        T
          { first_column = Table.first_column
          ; columns = (if col_groups then Table.with_column_groups else Table.all)
          }
    ;;
  end

  let computation config input (local_ graph) =
    let (T { columns; first_column }) = For_component.build config in
    let%sub { Prt_input.map; resize_column_widths_to_fit; row_height; _ } = input in
    let collate, key_rank =
      let collate =
        let%arr { filter
                ; order
                ; key_range
                ; rank_range
                ; resize_column_widths_to_fit = _
                ; map = _
                ; row_height = _
                }
          =
          input
        in
        { Incr_map_collate.Collate.filter; order; key_range; rank_range }
      in
      Table.collate
        ~filter_equal:phys_equal
        ~filter_to_predicate:Fn.id
        ~order_equal:phys_equal
        ~order_to_compare:Fn.id
        map
        collate
        graph
    in
    let columns = columns graph in
    let%sub { view; focus; range; _ } =
      Table.component
        (module Int)
        ~resize_column_widths_to_fit
        ~focus:
          (Table.Focus.By_cell
             { on_change = return (fun _ -> Effect.Ignore)
             ; compute_presence = (fun x _ -> return x)
             ; key_rank
             })
        ~row_height
        ~columns
        collate
        graph
    in
    let inject =
      let%arr focus in
      let module Focus_control = Table.Focus.By_cell in
      function
      | Action.Unfocus -> Focus_control.unfocus focus
      | Focus_up -> Focus_control.focus_up focus
      | Focus_down -> Focus_control.focus_down focus
      | Focus_left -> Focus_control.focus_left focus
      | Focus_right -> Focus_control.focus_right focus
      | Page_up -> Focus_control.page_up focus
      | Page_down -> Focus_control.page_down focus
      | Focus_first_column key -> (Focus_control.focus focus) key first_column
      | Focus_index_first_column idx -> (Focus_control.focus_index focus) idx first_column
    in
    let%arr view and range and inject in
    { Prt_output.view; range; inject }
  ;;

  let get_inject { Prt_output.inject; _ } = inject
end

module Scenarios = struct
  let focus_and_unfocus ~size ~in_range =
    let starting_map = Row.init_rows size in
    let not_ = if in_range then "" else "not " in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name =
        [%string
          "Focus by key (key %{not_}present) and unfocus in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          let index = if in_range then 1 else size + 1 in
          [ Interaction.inject (Action.Focus_first_column index)
          ; Interaction.inject Action.Unfocus
          ; Interaction.reset_model
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let focus_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Focus up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Focus_down; Interaction.inject Action.Focus_up ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  (* It doesn't make a lot of sense to have a really large number of columns for left/right
     focus benchmarks, so we just do it within the same size table as everything else which
     seems more realistic. *)
  let focus_left_and_right ~num_rows =
    let starting_map = Row.init_rows num_rows in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Focus left and right in a map with %{num_rows#Int} rows"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Focus_left; Interaction.inject Action.Focus_right ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let page_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Page up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Action.Page_down; Interaction.inject Action.Page_up ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let scroll ~size ~start ~stop ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name =
        [%string
          "Scroll %{window_size#Int}-wide window from %{start#Int} to %{stop#Int} and \
           back in %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Prt_input.scroll input ~start ~stop ~window_size
          ; Prt_input.scroll input ~start:stop ~stop:start ~window_size
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let apply_filters ~size ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Apply 4 filters and clear with %{size#Int} element map using \
           %{window_size#Int} window"]
    ; interaction =
        (fun input ->
          [ Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 2 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 3 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 4 = 0)
          ; Prt_input.apply_filter input (fun ~key ~data:_ -> key mod 5 = 0)
          ; Prt_input.clear_filter input
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let invert_ordering ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create starting_map
    ; test_name = [%string "Invert ordering of %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Prt_input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] a.Row.asize b.Row.asize)
                 })
          ; Prt_input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] b.Row.asize a.Row.asize)
                 })
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;

  let change_one_row ~size ~cells_to_change ~window_size =
    let starting_map = Row.init_rows size in
    let to_change_str =
      match cells_to_change with
      | `One -> "one cell"
      | `All -> "all cells"
    in
    { Scenario.initial = Prt_input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name = [%string "Randomly select a row, then change %{to_change_str} in it."]
    ; interaction =
        (fun input ->
          Prt_input.update_map input ~f:(fun current_map ->
            let row_to_change = Random.int size mod window_size in
            Map.update current_map row_to_change ~f:(function
              | None -> failwith "Only generating indexes within range"
              | Some row ->
                (match cells_to_change with
                 | `One -> { row with edge = Random.float_range (-3432432.3) 4324.342 }
                 | `All -> Row.of_int (Random.int size)))))
    }
  ;;

  (* [set_map] sets performs [num_sets * batch_size] map sets in total, with stabilization
     happening every [batch_size] changes. [window_size] specifies the size of the window,
     and the sets wrap around it. *)
  let set_map ~size ~num_sets ~batch_size ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Prt_input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Perform %{num_sets#Int} sets of %{batch_size#Int} items in a %{size#Int} \
           element map with %{window_size#Int}-wide window "]
    ; interaction =
        (fun input ->
          [ List.init num_sets ~f:(fun set_num ->
              Prt_input.update_map input ~f:(fun current_map ->
                let new_map, _ =
                  Fn.apply_n_times
                    ~n:batch_size
                    (fun (m, i) ->
                      let index = (set_num * batch_size) + i in
                      let new_ =
                        Map.set
                          m
                          ~key:(index mod window_size)
                          ~data:(Row.of_int (index + size))
                      in
                      new_, i + 1)
                    (current_map, 0)
                in
                new_map))
            |> Interaction.many_with_stabilizations
          ; Prt_input.update_map input ~f:(fun _ -> starting_map)
          ]
          |> Interaction.many_with_stabilizations)
    }
  ;;
end

let scenarios =
  let open Scenarios in
  [ focus_and_unfocus ~size:10 ~in_range:false
  ; focus_and_unfocus ~size:100 ~in_range:false
  ; focus_and_unfocus ~size:101 ~in_range:false
  ; focus_and_unfocus ~size:1000 ~in_range:false
  ; focus_and_unfocus ~size:10000 ~in_range:false
  ; focus_and_unfocus ~size:10 ~in_range:true
  ; focus_and_unfocus ~size:100 ~in_range:true
  ; focus_and_unfocus ~size:101 ~in_range:true
  ; focus_and_unfocus ~size:1000 ~in_range:true
  ; focus_and_unfocus ~size:10000 ~in_range:true
  ; focus_up_and_down ~size:10
  ; focus_up_and_down ~size:100
  ; focus_up_and_down ~size:101
  ; focus_up_and_down ~size:1000
  ; focus_up_and_down ~size:10000
  ; focus_left_and_right ~num_rows:10
  ; focus_left_and_right ~num_rows:100
  ; focus_left_and_right ~num_rows:101
  ; focus_left_and_right ~num_rows:1000
  ; focus_left_and_right ~num_rows:10000
  ; page_up_and_down ~size:10
  ; page_up_and_down ~size:100
  ; page_up_and_down ~size:101
  ; page_up_and_down ~size:1000
  ; page_up_and_down ~size:10000
  ; scroll ~size:100 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~size:100 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:1
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:10
  ; scroll ~size:1000 ~start:0 ~stop:9 ~window_size:100
  ; apply_filters ~size:100 ~window_size:10
  ; apply_filters ~size:101 ~window_size:10
  ; apply_filters ~size:1000 ~window_size:10
  ; apply_filters ~size:1000 ~window_size:50
  ; apply_filters ~size:10000 ~window_size:50
  ; apply_filters ~size:10000 ~window_size:100
  ; invert_ordering ~size:10
  ; invert_ordering ~size:100
  ; invert_ordering ~size:101
  ; invert_ordering ~size:1000
  ; change_one_row ~size:10 ~window_size:10 ~cells_to_change:`One
  ; change_one_row ~size:100 ~window_size:10 ~cells_to_change:`One
  ; change_one_row ~size:10000 ~window_size:10 ~cells_to_change:`One
  ; change_one_row ~size:10 ~window_size:10 ~cells_to_change:`All
  ; change_one_row ~size:100 ~window_size:10 ~cells_to_change:`All
  ; change_one_row ~size:10000 ~window_size:10 ~cells_to_change:`All
  ; set_map ~size:10 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:10 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:11 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:11 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:100 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:100 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:1 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:5 ~window_size:10
  ; set_map ~size:1000 ~num_sets:10 ~batch_size:10 ~window_size:100
  ]
;;
