open! Core
open Bonsai_web
open Bonsai_web_test
open Bonsai.Let_syntax
module Reorderable_list = Bonsai_web_reorderable_list
module Drag_and_drop = Bonsai_web_drag_and_drop
module Node = Vdom.Node

let small_list = List.range 0 3 |> List.map ~f:(fun x -> x, x) |> Int.Map.of_alist_exn

let component ?direction ?add_drop_target_for_appending list (local_ graph) =
  let dnd =
    Drag_and_drop.create
      ~source_id:(module Int)
      ~target_id:(module Int)
      ~on_drop:
        (Bonsai.return (fun source target ->
           Ui_effect.print_s [%message "drag" (source : int) (target : int)]))
      graph
  in
  let data =
    Bonsai.assoc
      (module Int)
      list
      ~f:(fun key data (local_ _graph) ->
        let item =
          let%arr data in
          Node.text (Int.to_string data)
        in
        Bonsai.both item key)
      graph
  in
  let sentinel = dnd >>| Drag_and_drop.sentinel in
  let dragged_element =
    Drag_and_drop.dragged_element
      dnd
      ~f:(fun item (local_ _graph) ->
        let%arr item in
        Node.text (Int.to_string item))
      graph
  in
  let source = dnd >>| Drag_and_drop.source in
  let list =
    Reorderable_list.list
      (module Int)
      ~dnd
      ?direction
      ?add_drop_target_for_appending
      ~container_extra_attrs:(Bonsai.return (Vdom.Attr.id "reorderable-list"))
      ~default_item_height:1
      data
      graph
  in
  let%arr sentinel and dragged_element and source and list in
  Node.div
    ~attrs:[ sentinel ~name:"dnd" ]
    [ Node.div ~attrs:[ source ~id:10 ] [ Node.text "10" ]; list; dragged_element ]
;;

let create_handle ?direction ?add_drop_target_for_appending list =
  Handle.create
    (Result_spec.vdom
       ~path_censoring_message:""
       ~filter_printed_attributes:(fun ~key ~data:_ ->
         match key with
         | "style.transform" -> true
         | key when String.is_prefix key ~prefix:"data-drag-target" -> true
         | _ -> false)
       Fn.id)
    (component ?direction ?add_drop_target_for_appending list)
;;

let%expect_test "how is it printed" =
  let handle = create_handle (Bonsai.return small_list) in
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
        <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
        <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
      </div>
    </div>
    |}]
;;

let dnd_action handle action =
  Bonsai_web_drag_and_drop_testing.run handle ~get_vdom:Fn.id ~name:"dnd" action
;;

let%expect_test "re-arrange" =
  let handle = create_handle (Bonsai.return small_list) in
  Handle.store_view handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    |}];
  dnd_action handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div>
    |}];
  dnd_action handle (Set_target (Some "2"));
  Handle.show_diff handle;
  [%expect {| |}];
  dnd_action handle (Set_target (Some "1"));
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 2 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
        <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div>
    |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
    (drag (source 0) (target 1))

      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
        </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div>
    |}]
;;

let%expect_test "drop onto empty list" =
  let handle = create_handle (Bonsai.return Int.Map.empty) in
  Handle.store_view handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    |}];
  dnd_action handle (Start_drag "0");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
    -|  <div> </div>
    +|  <div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|  </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div>
    |}];
  dnd_action handle (Set_target (Some "10"));
  Handle.show_diff handle;
  [%expect {| |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  (* As expected, dropping an element on a non-existent target does not add the item to
     the list. *)
  [%expect
    {|
    (drag (source 0) (target 10))

      <div>
        <div> 10 </div>
    -|  <div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|  </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|  <div> </div>
      </div>
    |}]
;;

let%expect_test "add item from outside the list" =
  let handle = create_handle (Bonsai.return small_list) in
  Handle.store_view handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    |}];
  dnd_action handle (Start_drag "10");
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    +|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
    +|  <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
      </div>
    |}];
  dnd_action handle (Set_target (Some "1"));
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div style={ transform: translateY(3px) translateX(0px); }> 2 </div>
          <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
        </div>
        <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
      </div>
    |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
    (drag (source 10) (target 1))

      <div>
        <div> 10 </div>
        <div>
    -|    <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }> 1 </div>
    -|    <div style={ transform: translateY(3px) translateX(0px); }> 2 </div>
    -|    <div data-drag-target="3" style={ transform: translateY(3px) translateX(0px); }> </div>
    -|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|    <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> </div>
    +|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> </div>
    +|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
        </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
      </div>
    |}]
;;

let%expect_test "re-arrange" =
  let handle = create_handle (Bonsai.return small_list) in
  Handle.store_view handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    |}];
  Bonsai_web_element_size_hooks_testing.Bulk_size_tracker.change_sizes
    handle
    ~get_vdom:Fn.id
    [ { selector = "div > div :nth-child(1)"; height = 2.0; width = 0. }
    ; { selector = "div > div :nth-child(2)"; height = 4.0; width = 0. }
    ];
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div> 10 </div>
        <div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|    <div data-drag-target="1" style={ transform: translateY(1px) translateX(0px); }> 1 </div>
    +|    <div data-drag-target="1" style={ transform: translateY(2px) translateX(0px); }> 1 </div>
    -|    <div data-drag-target="2" style={ transform: translateY(2px) translateX(0px); }> 2 </div>
    +|    <div data-drag-target="2" style={ transform: translateY(6px) translateX(0px); }> 2 </div>
        </div>
      </div>
    |}]
;;

let set_container_width handle width =
  Handle.trigger_hook
    handle
    ~selector:"#reorderable-list > div"
    ~name:"size_tracker"
    Bonsai_web_element_size_hooks.Size_tracker.For_testing.type_id
    { border_box = { width; height = 100. }; content_box = { width; height = 100. } }
    ~get_vdom:Fn.id
;;

let set_horizontal_item_sizes handle =
  Bonsai_web_element_size_hooks_testing.Bulk_size_tracker.change_sizes
    handle
    ~get_vdom:Fn.id
    [ { selector = "#reorderable-list > div > div:nth-child(1)"
      ; width = 40.
      ; height = 10.
      }
    ; { selector = "#reorderable-list > div > div:nth-child(2)"
      ; width = 35.
      ; height = 20.
      }
    ; { selector = "#reorderable-list > div > div:nth-child(3)"
      ; width = 50.
      ; height = 15.
      }
    ; { selector = "#reorderable-list > div > div:nth-child(4)"
      ; width = 45.
      ; height = 12.
      }
    ]
;;

let create_horizontal_handle ?add_drop_target_for_appending () =
  let input =
    List.range 0 4 |> List.map ~f:(fun x -> x, x) |> Int.Map.of_alist_exn |> Bonsai.return
  in
  let handle =
    create_handle ~direction:`Horizontal ?add_drop_target_for_appending input
  in
  Handle.store_view handle;
  set_container_width handle 100.;
  set_horizontal_item_sizes handle;
  handle
;;

let%expect_test "horizontal lists wrap and respond to container resizes" =
  let handle = create_horizontal_handle () in
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
          <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> 1 </div>
          <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> 2 </div>
          <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> 3 </div>
        </div>
      </div>
    </div>
    |}];
  set_container_width handle 80.;
  Handle.show_diff ~diff_context:0 handle;
  [%expect
    {|
    -|      <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> 3 </div>
    +|      <div data-drag-target="3" style={ transform: translateY(35px) translateX(0px); }> 3 </div>
    |}]
;;

let%expect_test "horizontal lists reflow while dragging across rows" =
  let handle = create_horizontal_handle () in
  dnd_action handle (Start_drag "0");
  dnd_action handle (Set_target (Some "2"));
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div>
          <div style={ transform: translateY(20px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
          <div style={ transform: translateY(0px) translateX(35px); }> 2 </div>
          <div style={ transform: translateY(20px) translateX(40px); }> 3 </div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> </div>
          <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> </div>
        </div>
      </div>
      <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    </div>
    |}];
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
    (drag (source 0) (target 2))

      <div>
        <div> 10 </div>
        <div>
          <div>
    -|      <div style={ transform: translateY(20px) translateX(0px); }> 0 </div>
    -|      <div style={ transform: translateY(0px) translateX(0px); }> 1 </div>
    -|      <div style={ transform: translateY(0px) translateX(35px); }> 2 </div>
    -|      <div style={ transform: translateY(20px) translateX(40px); }> 3 </div>
    -|      <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
    +|      <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
    -|      <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> </div>
    +|      <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> 1 </div>
    -|      <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> </div>
    +|      <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> 2 </div>
    -|      <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> </div>
    +|      <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> 3 </div>
          </div>
        </div>
    -|  <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
      </div>
    |}]
;;

let%expect_test "horizontal lists make space for an external item" =
  let handle = create_horizontal_handle () in
  dnd_action handle (Start_drag "10");
  dnd_action handle (Set_target (Some "3"));
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(0px) translateX(40px); }> 1 </div>
          <div style={ transform: translateY(20px) translateX(0px); }> 2 </div>
          <div style={ transform: translateY(20px) translateX(51px); }> 3 </div>
          <div data-drag-target="4" style={ transform: translateY(20px) translateX(95px); }> </div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> </div>
          <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> </div>
        </div>
      </div>
      <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
    </div>
    |}]
;;

let%expect_test "horizontal list omits the external append target when disabled" =
  let handle = create_horizontal_handle ~add_drop_target_for_appending:false () in
  dnd_action handle (Start_drag "10");
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }> 0 </div>
          <div style={ transform: translateY(0px) translateX(40px); }> 1 </div>
          <div style={ transform: translateY(20px) translateX(0px); }> 2 </div>
          <div style={ transform: translateY(20px) translateX(50px); }> 3 </div>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> </div>
          <div data-drag-target="1" style={ transform: translateY(0px) translateX(40px); }> </div>
          <div data-drag-target="2" style={ transform: translateY(20px) translateX(0px); }> </div>
          <div data-drag-target="3" style={ transform: translateY(20px) translateX(50px); }> </div>
        </div>
      </div>
      <div style={ transform: translateY(0px) translateX(0px); }> 10 </div>
    </div>
    |}]
;;

let%expect_test "horizontal layout rounds item dimensions up" =
  let input = [ 0, 0; 1, 1 ] |> Int.Map.of_alist_exn |> Bonsai.return in
  let handle = create_handle ~direction:`Horizontal input in
  Handle.store_view handle;
  set_container_width handle 100.9;
  Bonsai_web_element_size_hooks_testing.Bulk_size_tracker.change_sizes
    handle
    ~get_vdom:Fn.id
    [ { selector = "#reorderable-list > div > div:nth-child(1)"
      ; width = 50.1
      ; height = 10.1
      }
    ; { selector = "#reorderable-list > div > div:nth-child(2)"
      ; width = 50.1
      ; height = 10.1
      }
    ];
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div> 10 </div>
      <div>
        <div>
          <div data-drag-target="0" style={ transform: translateY(0px) translateX(0px); }> 0 </div>
          <div data-drag-target="1" style={ transform: translateY(11px) translateX(0px); }> 1 </div>
        </div>
      </div>
    </div>
    |}]
;;

let component input (local_ graph) =
  let%sub _, view =
    Reorderable_list.simple
      (module Int)
      ~sentinel_name:"dnd"
      ~default_item_height:1
      ~render:(fun ~index:_ ~source i (local_ _graph) ->
        let%arr i and source in
        (), Vdom.Node.div ~attrs:[ source ] [ Vdom.Node.text (Int.to_string i) ])
      input
      graph
  in
  view
;;

let create_handle input =
  Handle.create
    (Result_spec.vdom
       ~filter_printed_attributes:(fun ~key ~data:_ ->
         match key with
         | "style.transform" -> true
         | _ -> false)
       Fn.id)
    (component input)
;;

let%expect_test "re-arrange" =
  let input = List.range 0 3 |> List.map ~f:Fn.id |> Int.Set.of_list |> Bonsai.return in
  let handle = create_handle input in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div>
        <div style={ transform: translateY(0px) translateX(0px); }>
          <div> 0 </div>
        </div>
        <div style={ transform: translateY(1px) translateX(0px); }>
          <div> 1 </div>
        </div>
        <div style={ transform: translateY(2px) translateX(0px); }>
          <div> 2 </div>
        </div>
      </div>
    </div>
    |}];
  dnd_action handle (Start_drag "0");
  dnd_action handle (Set_target (Some "2"));
  dnd_action handle Finish_drag;
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div>
    +|    <div style={ transform: translateY(2px) translateX(0px); }>
    +|      <div> 0 </div>
    +|    </div>
          <div style={ transform: translateY(0px) translateX(0px); }>
    -|      <div> 0 </div>
    +|      <div> 1 </div>
          </div>
          <div style={ transform: translateY(1px) translateX(0px); }>
    -|      <div> 1 </div>
    -|    </div>
    -|    <div style={ transform: translateY(2px) translateX(0px); }>
            <div> 2 </div>
          </div>
        </div>
      </div>
    |}]
;;

let%expect_test "dynamically extend list" =
  let input_var =
    List.range 0 3 |> List.map ~f:Fn.id |> Int.Set.of_list |> Bonsai.Expert.Var.create
  in
  let input = Bonsai.Expert.Var.value input_var in
  let handle = create_handle input in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    <div>
      <div>
        <div style={ transform: translateY(0px) translateX(0px); }>
          <div> 0 </div>
        </div>
        <div style={ transform: translateY(1px) translateX(0px); }>
          <div> 1 </div>
        </div>
        <div style={ transform: translateY(2px) translateX(0px); }>
          <div> 2 </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.update input_var ~f:(fun x -> Set.add x 20);
  Handle.recompute_view handle;
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <div>
          <div style={ transform: translateY(0px) translateX(0px); }>
            <div> 0 </div>
          </div>
          <div style={ transform: translateY(1px) translateX(0px); }>
            <div> 1 </div>
          </div>
          <div style={ transform: translateY(2px) translateX(0px); }>
            <div> 2 </div>
          </div>
    +|    <div style={ transform: translateY(3px) translateX(0px); }>
    +|      <div> 20 </div>
    +|    </div>
        </div>
      </div>
    |}]
;;

type result =
  (int * int) list * Vdom.Node.t * (int Reorderable_list.Action.t -> unit Effect.t)

let%expect_test "removing an item should shift the rank of everything else" =
  let component (local_ graph) =
    Reorderable_list.with_inject
      (module Int)
      ~sentinel_name:"dnd"
      ~default_item_height:1
      (fun ~index ~source:_ _ (local_ _graph) ->
        let%arr index in
        index, Vdom.Node.None)
      graph
  in
  let handle =
    Handle.create
      (module struct
        type t = result
        type incoming = int Reorderable_list.Action.t

        let incoming (_, _, inject) = inject
        let view (view, _, _) = Sexp.to_string_hum ([%sexp_of: (int * int) list] view)
      end : Result_spec.S
        with type t = result
         and type incoming = int Reorderable_list.Action.t)
      component
  in
  Handle.recompute_view handle;
  Handle.show handle;
  [%expect
    {|
    adding window event listener
    adding window event listener
    ()
    |}];
  (* The result sexp shows the mapping from the item's key to its rank in the output
     order. Note that removing the 0 item makes the rank of the 1 item change from 1 to 0,
     as it should.

     In other words, removing an item from the ranking should cause any items below it to
     shift upward in the ranking by one. *)
  Handle.do_actions handle [ [ Set 0; Set 1 ] ];
  Handle.show handle;
  [%expect {| ((0 0) (1 1)) |}];
  Handle.do_actions handle [ [ Remove 0 ] ];
  Handle.show handle;
  [%expect {| ((1 0)) |}]
;;
