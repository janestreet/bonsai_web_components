open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Prt = Bonsai_web_ui_partial_render_table
module Table = Prt.Basic
module Row = Symbol_table.Row

module Which_column = struct
  type t =
    | First
    | Middle
    | Last
  [@@deriving compare, equal, sexp, enumerate]
end

let dynamic_cells ~input which_column : (int, Row.t, _) Table.Columns.t =
  let column_helper
    (type a)
    (module M : Stringable with type t = a)
    ?(depend_on_input = false)
    (field : (_, a) Field.t)
    =
    Table.Columns.Dynamic_cells.column
      ~header:(return (Vdom.Node.text (Fieldslib.Field.name field)))
      ~cell:(fun ~key:_ ~data _graph ->
        if depend_on_input
        then (
          let%arr data and input in
          let data_str = M.to_string (Field.get field data) in
          {%html|<div>#{data_str} (%{input#Int})</div>|})
        else (
          let%arr data in
          let data_str = M.to_string (Field.get field data) in
          {%html|<div>#{data_str}</div>|}))
      ()
  in
  [ column_helper
      (module String)
      Row.Fields.symbol
      ~depend_on_input:(Which_column.equal which_column First)
  ; column_helper (module Float) Row.Fields.edge
  ; column_helper (module Float) Row.Fields.max_edge
  ; column_helper
      (module Int)
      Row.Fields.bsize
      ~depend_on_input:(Which_column.equal which_column Middle)
  ; column_helper (module Float) Row.Fields.bid
  ; column_helper (module Float) Row.Fields.ask
  ; column_helper
      (module Int)
      Row.Fields.asize
      ~depend_on_input:(Which_column.equal which_column Last)
  ]
  |> Table.Columns.Dynamic_cells.lift
;;

let dynamic_cols ~input which_column : (int, Row.t, _) Table.Columns.t =
  let columns =
    let%arr input in
    let column_helper
      (type a)
      (module M : Stringable with type t = a)
      ?(depend_on_input = false)
      (field : (_, a) Field.t)
      =
      let name = Fieldslib.Field.name field in
      Table.Columns.Dynamic_columns.column
        ~header:(Vdom.Node.text name)
        ~cell:(fun ~key:_ ~data ->
          if depend_on_input
          then (
            let data_str = M.to_string (Field.get field data) in
            {%html|<div>#{data_str} (%{input#Int})</div>|})
          else (
            let data_str = M.to_string (Field.get field data) in
            {%html|<div>#{data_str}</div>|}))
        ()
    in
    [ column_helper
        (module String)
        Row.Fields.symbol
        ~depend_on_input:(Which_column.equal which_column First)
    ; column_helper (module Float) Row.Fields.edge
    ; column_helper (module Float) Row.Fields.max_edge
    ; column_helper
        (module Int)
        Row.Fields.bsize
        ~depend_on_input:(Which_column.equal which_column Middle)
    ; column_helper (module Float) Row.Fields.bid
    ; column_helper (module Float) Row.Fields.ask
    ; column_helper
        (module Int)
        Row.Fields.asize
        ~depend_on_input:(Which_column.equal which_column Last)
    ]
  in
  Table.Columns.Dynamic_columns.lift columns
;;

module Which_api = struct
  type t =
    | Dynamic_cells
    | Dynamic_cols
  [@@deriving compare, sexp, enumerate]
end

module Input = struct
  type t =
    { col_dependency : int
    ; data : Symbol_table.Row.t Int.Map.t
    }
end

type t =
  { which_column : Which_column.t
  ; which_api : Which_api.t
  }
[@@deriving compare, sexp, enumerate]

let name { which_column; which_api } =
  let column =
    match which_column with
    | First -> "first"
    | Middle -> "middle"
    | Last -> "last"
  in
  let api =
    match which_api with
    | Dynamic_cells -> "dynamic cells"
    | Dynamic_cols -> "dynamic cols"
  in
  [%string "%{api}: %{column} col"]
;;

let computation { which_column; which_api } input (local_ graph) =
  let%sub { Input.col_dependency; data } = input in
  let columns =
    match which_api with
    | Dynamic_cells -> dynamic_cells ~input:col_dependency which_column
    | Dynamic_cols -> dynamic_cols ~input:col_dependency which_column
  in
  let%sub { view; _ } =
    Table.component
      (module Int)
      ~focus:(Table.Focus.By_cell { on_change = return (fun _ -> Effect.Ignore) })
      ~row_height:(return (`Px 30))
      ~columns
      data
      graph
  in
  view
;;

let all_computations = lazy (List.map all ~f:(fun t -> name t, computation t))

module Scenarios = struct
  module Prt_input = Input
  open Bonsai_bench_scenario

  let incr_dependency ~num_rows ~num_incrs =
    let starting_map = Row.init_rows num_rows in
    { Scenario.initial = { Prt_input.col_dependency = 0; data = starting_map }
    ; test_name = [%string "Update col dep %{num_incrs#Int} times: %{num_rows#Int} rows"]
    ; interaction =
        (fun input ->
          List.init num_incrs ~f:(fun _ ->
            Interaction.update_input input ~f:(fun input ->
              { input with col_dependency = input.col_dependency + 7 }))
          |> Interaction.many_with_recomputes)
    }
  ;;
end

let scenarios =
  let open Scenarios in
  [ incr_dependency ~num_rows:10 ~num_incrs:2
  ; incr_dependency ~num_rows:100 ~num_incrs:2
  ; incr_dependency ~num_rows:101 ~num_incrs:2
  ; incr_dependency ~num_rows:1000 ~num_incrs:2
  ; incr_dependency ~num_rows:10000 ~num_incrs:2
  ; incr_dependency ~num_rows:10 ~num_incrs:10
  ; incr_dependency ~num_rows:100 ~num_incrs:10
  ; incr_dependency ~num_rows:101 ~num_incrs:10
  ; incr_dependency ~num_rows:1000 ~num_incrs:10
  ; incr_dependency ~num_rows:10000 ~num_incrs:10
  ]
;;
