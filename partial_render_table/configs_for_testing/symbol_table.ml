open! Core
open! Bonsai_web
open Bonsai_bench_scenario
open Sharable

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

module Scenarios = struct
  let focus_and_unfocus ~size ~in_range =
    let starting_map = Row.init_rows size in
    let not_ = if in_range then "" else "not " in
    { Scenario.initial = Input.create starting_map
    ; test_name =
        [%string
          "Focus by key (key %{not_}present) and unfocus in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          let key = if in_range then 1 else size + 1 in
          [ Interaction.inject (Navigation_action.Focus_first_column key)
          ; Interaction.inject Navigation_action.Unfocus
          ; Interaction.reset_model
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let focus_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Input.create starting_map
    ; test_name = [%string "Focus up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Navigation_action.Focus_down
          ; Interaction.inject Navigation_action.Focus_up
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  (* It doesn't make a lot of sense to have a really large number of columns for
     left/right focus benchmarks, so we just do it within the same size table as
     everything else which seems more realistic. *)
  let focus_left_and_right ~num_rows =
    let starting_map = Row.init_rows num_rows in
    { Scenario.initial = Input.create starting_map
    ; test_name = [%string "Focus left and right in a map with %{num_rows#Int} rows"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Navigation_action.Focus_left
          ; Interaction.inject Navigation_action.Focus_right
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let page_up_and_down ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Input.create starting_map
    ; test_name = [%string "Page up and down in %{size#Int} element map"]
    ; interaction =
        (fun _ ->
          [ Interaction.inject Navigation_action.Page_down
          ; Interaction.inject Navigation_action.Page_up
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let scroll ~size ~start ~stop ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Input.create starting_map
    ; test_name =
        [%string
          "Scroll %{window_size#Int}-wide window from %{start#Int} to %{stop#Int} and \
           back in %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Input.scroll input ~start ~stop ~window_size
          ; Input.scroll input ~start:stop ~stop:start ~window_size
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let apply_filters ~size ~window_size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Apply 4 filters and clear with %{size#Int} element map using \
           %{window_size#Int} window"]
    ; interaction =
        (fun input ->
          [ Input.apply_filter input (fun ~key ~data:_ -> key mod 2 = 0)
          ; Input.apply_filter input (fun ~key ~data:_ -> key mod 3 = 0)
          ; Input.apply_filter input (fun ~key ~data:_ -> key mod 4 = 0)
          ; Input.apply_filter input (fun ~key ~data:_ -> key mod 5 = 0)
          ; Input.clear_filter input
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let invert_ordering ~size =
    let starting_map = Row.init_rows size in
    { Scenario.initial = Input.create starting_map
    ; test_name = [%string "Invert ordering of %{size#Int} element map"]
    ; interaction =
        (fun input ->
          [ Input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] a.Row.asize b.Row.asize)
                 })
          ; Input.set_order
              input
              (Custom_by_key_and_value
                 { compare =
                     (fun (_, a) (_, b) -> [%compare: int] b.Row.asize a.Row.asize)
                 })
          ]
          |> Interaction.many_with_recomputes)
    }
  ;;

  let change_one_row ~size ~cells_to_change ~window_size =
    let starting_map = Row.init_rows size in
    let to_change_str =
      match cells_to_change with
      | `One -> "one cell"
      | `All -> "all cells"
    in
    { Scenario.initial = Input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Randomly select a row out of a table with %{size #Int} rows and a window of \
           %{window_size #Int}, then change %{to_change_str} in it."]
    ; interaction =
        (fun input ->
          Input.update_map input ~f:(fun current_map ->
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
    { Scenario.initial = Input.create ~rank_range:(To (window_size - 1)) starting_map
    ; test_name =
        [%string
          "Perform %{num_sets#Int} sets of %{batch_size#Int} items in a %{size#Int} \
           element map with %{window_size#Int}-wide window "]
    ; interaction =
        (fun input ->
          [ List.init num_sets ~f:(fun set_num ->
              Input.update_map input ~f:(fun current_map ->
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
            |> Interaction.many_with_recomputes
          ; Input.update_map input ~f:(fun _ -> starting_map)
          ]
          |> Interaction.many_with_recomputes)
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

let startup_inputs ns =
  List.map ns ~f:(fun size ->
    Int.to_string size, Sharable.Input.create (Row.init_rows size))
;;
