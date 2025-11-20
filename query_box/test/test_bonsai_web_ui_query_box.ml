open! Core
open Bonsai_web
open Bonsai_web_test
open Vdom
open Bonsai.Let_syntax

let get_vdom = Bonsai_web_ui_query_box.view
let fruits = [ "apple"; "orange"; "kiwi"; "dragon fruit" ]
let items = List.mapi fruits ~f:Tuple2.create |> Int.Map.of_alist_exn

module Spec = struct
  type t = int Bonsai_web_ui_query_box.t
  type incoming = Nothing.t

  let incoming _ incoming = Nothing.unreachable_code incoming

  let view box =
    let html =
      Virtual_dom_test_helpers.Node_helpers.to_string_html
        ~filter_printed_attributes:(fun ~key ~data:_ ->
          match key with
          | "class" | "data-test" -> true
          | _ -> false)
        ~censor_paths:true
        ~censor_hash:true
        (Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn (get_vdom box))
    in
    let focused_item =
      match box.Bonsai_web_ui_query_box.focused_item with
      | None -> "None"
      | Some item -> Int.to_string item
    in
    [%string "Focused item: %{focused_item}\n\n%{html}"]
  ;;
end

let create
  ?expand_direction
  ?on_blur
  ?on_focus
  ~on_hover_item
  ?(items = items)
  ?(modify_input_on_select = fun _ _ -> "")
  ()
  =
  let component (local_ graph) =
    Bonsai_web_ui_query_box.create
      (module Int)
      ?expand_direction:(Option.map expand_direction ~f:Bonsai.return)
      ~max_visible_items:(Bonsai.return 3)
      ~f:(fun query (local_ _graph) ->
        let%arr query in
        Map.filter items ~f:(String.is_prefix ~prefix:query) |> Map.map ~f:Node.text)
      ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
      ?on_blur
      ?on_focus:(Option.map on_focus ~f:Bonsai.return)
      ~on_hover_item
      ~on_select:(Bonsai.return (fun item -> Effect.print_s [%message (item : int)]))
      ~modify_input_on_select:(Bonsai.return modify_input_on_select)
      ()
      graph
  in
  Handle.create (module Spec) component
;;

let input_text handle text = Handle.input_text handle ~get_vdom ~selector:"input" ~text

let keydown handle ?shift_key_down key =
  Handle.keydown ?shift_key_down handle ~get_vdom ~selector:"input" ~key
;;

let blur ?related_target handle selector =
  Handle.blur handle ?related_target ~get_vdom ~selector
;;

let focus handle = Handle.focus handle ~get_vdom ~selector:"input"

let%expect_test "changing text does filtering" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  Handle.show handle;
  [%expect
    {|
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}];
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "a";
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "keybindings and filtering behavior" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  Handle.store_view handle;
  (* Focusing should open the suggestion list *)
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  (* Escape should close the suggestion list *)
  keydown handle Escape;
  Handle.show handle;
  [%expect
    {|
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}];
  (* Down should open the suggestion list *)
  keydown handle ArrowDown;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  (* Tab should cycle to the next next item. *)
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 1

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div class="focused-item"> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  (* Tab should cycle to the next next item. (again) *)
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div> orange </div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  (* Closing and reopening the suggestion list resets what item is focused. *)
  keydown handle Escape;
  Handle.recompute_view handle;
  keydown handle Enter;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  (* Advancing to the next item. *)
  keydown handle Tab;
  Handle.show_diff handle;
  [%expect
    {|
    ("default prevented" (key Tab))

    -|Focused item: 0
    +|Focused item: 1

      <div>
        <input/>
        <div data-test="query-box-item-container">
          <div>
    -|      <div class="focused-item"> apple </div>
    -|      <div> orange </div>
    +|      <div> apple </div>
    +|      <div class="focused-item"> orange </div>
            <div> kiwi </div>
          </div>
        </div>
      </div>
    |}];
  (* Filtering down to a focused item, and then removing the filter should have no effect
     on the selection, since it isn't based in the an integer index. *)
  input_text handle "o";
  input_text handle "";
  Handle.show_diff handle;
  [%expect {| |}];
  (* Filtering such that the focused item gets removed should set the focused item to its
     nearest neighbor, even if the removed item comes back into the map. *)
  input_text handle "a";
  input_text handle "";
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "";
  keydown handle Escape;
  Handle.show handle;
  [%expect
    {|
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}];
  keydown handle ArrowUp;
  (* UpArrow should open the suggestion list, but with the selection set to the bottom. *)
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowUp))
    Focused item: 3

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> orange </div>
          <div> kiwi </div>
          <div class="focused-item"> dragon fruit </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle Escape;
  Handle.recompute_view handle;
  keydown handle Escape;
  (* Double Escape blurs the input *)
  Handle.show handle;
  [%expect
    {|
    ("blur effect for" query-box)
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}]
;;

let%expect_test "inputting text twice in the same frame shouldn't be a problem" =
  let xs = [ "ab"; "ac"; "de" ] in
  let items = List.mapi xs ~f:Tuple2.create |> Int.Map.of_alist_exn in
  let handle =
    create
      ~items
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  Handle.show handle;
  [%expect
    {|
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}];
  focus handle;
  Handle.recompute_view handle;
  keydown handle Tab;
  Handle.recompute_view handle;
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> ab </div>
          <div> ac </div>
          <div class="focused-item"> de </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "a";
  input_text handle "ac";
  Handle.show handle;
  [%expect
    {|
    Focused item: 1

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> ac </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "partial-rendering" =
  let fruits =
    String.Map.of_alist_exn
      (List.map
         ~f:(fun x -> x, ())
         [ "apple"
         ; "apricot"
         ; "avocado"
         ; "banana"
         ; "blackberry"
         ; "blueberry"
         ; "breadfruit"
         ; "cantaloupe"
         ; "clementine"
         ; "fig"
         ; "grapefruit"
         ; "orange"
         ; "raspberry"
         ; "strawberry"
         ; "tangerine"
         ; "watermelon"
         ])
  in
  let component (local_ graph) =
    Bonsai_web_ui_query_box.create
      (module String)
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ~on_select:(Bonsai.return (fun _ -> Effect.Ignore))
      ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
      ~max_visible_items:(Bonsai.return 4)
      ~f:(fun query (local_ _graph) ->
        let%arr query in
        Map.filter_mapi fruits ~f:(fun ~key:fruit ~data:() ->
          if Fuzzy_match.is_match ~char_equal:Char.Caseless.equal fruit ~pattern:query
          then Some (Node.text fruit)
          else None))
      ()
      graph
  in
  let handle =
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
           | "class" -> true
           | _ -> false)
         get_vdom)
      component
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div>
        <div>
          <div class="focused-item"> apple </div>
          <div> apricot </div>
          <div> avocado </div>
          <div> banana </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "w";
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div>
        <div>
          <div class="focused-item"> strawberry </div>
          <div> watermelon </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    <div>
      <input/>
      <div>
        <div>
          <div> strawberry </div>
          <div class="focused-item"> watermelon </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "";
  (* Even after unfiltering the list of fruits, "watermelon" remains focused, and the list
     is offset to ensure that it is visible. *)
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div>
        <div>
          <div> raspberry </div>
          <div> strawberry </div>
          <div> tangerine </div>
          <div class="focused-item"> watermelon </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle ArrowDown;
  (* ArrowDown (or Tab) should wrap around to the top when it hits the bottom. *)
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    <div>
      <input/>
      <div>
        <div>
          <div class="focused-item"> apple </div>
          <div> apricot </div>
          <div> avocado </div>
          <div> banana </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle ArrowUp;
  (* ArrowUp (or Shift-Tab) should wrap around to the bottom when it hits the top. *)
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowUp))
    <div>
      <input/>
      <div>
        <div>
          <div> raspberry </div>
          <div> strawberry </div>
          <div> tangerine </div>
          <div class="focused-item"> watermelon </div>
        </div>
      </div>
    </div>
    |}];
  (* Move focus up several times (using both Shift-Tab and Arrow keys). *)
  keydown handle ~shift_key_down:true Tab;
  keydown handle ArrowUp;
  keydown handle ArrowUp;
  keydown handle ArrowUp;
  keydown handle ArrowUp;
  (* Observe that the focused item is at the top of list of completions. *)
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    ("default prevented" (key ArrowUp))
    ("default prevented" (key ArrowUp))
    ("default prevented" (key ArrowUp))
    ("default prevented" (key ArrowUp))
    <div>
      <input/>
      <div>
        <div>
          <div class="focused-item"> grapefruit </div>
          <div> orange </div>
          <div> raspberry </div>
          <div> strawberry </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "tabbing one item visible should exit First_item mode" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  input_text handle "kiwi";
  Handle.show handle;
  [%expect
    {|
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle Tab;
  input_text handle "";
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> orange </div>
          <div class="focused-item"> kiwi </div>
          <div> dragon fruit </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "shift-tabbing one item visible should exit First_item mode" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  input_text handle "kiwi";
  Handle.show handle;
  [%expect
    {|
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  keydown handle ~shift_key_down:true Tab;
  input_text handle "";
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div> orange </div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "[expand_direction=Up] reverses list order and keybindings" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ~expand_direction:Up
      ()
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 3

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> dragon fruit </div>
          <div> kiwi </div>
          <div> orange </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> dragon fruit </div>
          <div class="focused-item"> kiwi </div>
          <div> orange </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle ArrowDown;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    Focused item: 1

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> dragon fruit </div>
          <div> kiwi </div>
          <div class="focused-item"> orange </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle ~shift_key_down:true Tab;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Tab))
    Focused item: 2

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> dragon fruit </div>
          <div class="focused-item"> kiwi </div>
          <div> orange </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle ArrowUp;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowUp))
    Focused item: 3

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> dragon fruit </div>
          <div> kiwi </div>
          <div> orange </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  keydown handle ArrowUp;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowUp))
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}]
;;

let%expect_test "The element containing all the items should be focusable without \
                 closing the list of items"
  =
  let handle =
    create
      ~on_blur:(Bonsai.return (Effect.print_s [%message "on_blur called"]))
      ~expand_direction:Up
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  (* Blurring the input to focus the list of items doesn't close the list of items. *)
  blur ~related_target:"[data-test=query-box-item-container]" handle "input";
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  (* However, blurring the input without focusing something else *will* close the list of
     items. *)
  blur handle "input";
  Handle.show handle;
  [%expect
    {|
    "on_blur called"
    Focused item: None

    <div>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
      <input/>
    </div>
    |}];
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  (* Also, blurring the item container to focus the input will also not close the list of
     items. *)
  blur ~related_target:"input" handle "[data-test=query-box-item-container]";
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <div data-test="query-box-item-container">
        <div>
          <div> kiwi </div>
          <div> orange </div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
      <input/>
    </div>
    |}];
  (* But blurring the item container without focusing something else *will* close the list
     of items. *)
  blur handle "[data-test=query-box-item-container]";
  Handle.show handle;
  [%expect
    {|
    "on_blur called"
    Focused item: None

    <div>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
      <input/>
    </div>
    |}]
;;

let%expect_test "clicking on item invokes the callback and closes the list" =
  let handle =
    create
      ~expand_direction:Down
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  Handle.click_on handle ~get_vdom ~selector:".focused-item";
  Handle.show handle;
  [%expect
    {|
    (item 0)
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}]
;;

let%expect_test "mouseenter on an item selects it, and mousewheel scrolls up and down" =
  let handle =
    create
      ~expand_direction:Down
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  Handle.mouseenter
    handle
    ~get_vdom
    ~selector:"[data-test=query-box-item-container] > div > div:nth-child(3)";
  Handle.show handle;
  [%expect
    {|
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div> orange </div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "mousewheel on an item selects it" =
  let handle =
    create
      ~expand_direction:Down
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  focus handle;
  Handle.show handle;
  [%expect
    {|
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  Handle.mouseenter
    handle
    ~get_vdom
    ~selector:"[data-test=query-box-item-container] > div > div:nth-child(3)";
  Handle.show handle;
  [%expect
    {|
    Focused item: 2

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div> orange </div>
          <div class="focused-item"> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  Handle.wheel
    handle
    ~get_vdom
    ~selector:"[data-test=query-box-item-container]"
    ~delta_y:(-1.0);
  Handle.show handle;
  [%expect
    {|
    Focused item: 1

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> apple </div>
          <div class="focused-item"> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test {|key stays on the same item if the list of items changes (simple string map)|}
  =
  let items =
    Bonsai.Expert.Var.create (Int.Map.of_alist_exn [ 1, "a"; 3, "c"; 5, "e" ])
  in
  let component (local_ graph) =
    Bonsai_web_ui_query_box.create
      (module Int)
      ~max_visible_items:(Bonsai.return 3)
      ~f:(fun query (local_ _graph) ->
        let%arr query
        and items = Bonsai.Expert.Var.value items in
        Map.filter items ~f:(String.is_prefix ~prefix:query) |> Map.map ~f:Node.text)
      ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
      ~on_select:(Bonsai.return (fun item -> Effect.print_s [%message (item : int)]))
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
      graph
  in
  let handle =
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
           | "class" | "data-test" -> true
           | _ -> false)
         get_vdom)
      component
  in
  focus handle;
  keydown handle ArrowDown;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> a </div>
          <div class="focused-item"> c </div>
          <div> e </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.update items ~f:(fun map -> Map.set map ~key:2 ~data:"b");
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> b </div>
          <div class="focused-item"> c </div>
          <div> e </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.update items ~f:(fun map -> Map.set map ~key:4 ~data:"d");
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> b </div>
          <div class="focused-item"> c </div>
          <div> d </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test {|key stays on the same item if the list of items changes (collation)|} =
  let items =
    Bonsai.Expert.Var.create (Int.Map.of_alist_exn [ 1, "a"; 3, "c"; 5, "e" ])
  in
  let component =
    Bonsai_web_ui_query_box.create
      (module Bonsai_web_ui_query_box.Collate_map_with_score.Scored_key.M (Int))
      ~max_visible_items:(Bonsai.return 3)
      ~f:(fun query ->
        Bonsai_web_ui_query_box.Collate_map_with_score.collate
          (module Int)
          (Bonsai.Expert.Var.value items)
          query
          ~preprocess:(fun ~key:_ ~data -> data)
          ~score:(fun _query _item -> 1)
          ~query_is_as_strict:(fun q ~as_ -> String.is_substring q ~substring:as_)
          ~to_result:(fun item ~key:_ ~data:_ -> Node.text item))
      ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
      ~on_select:(Bonsai.return (fun (_, item) -> Effect.print_s [%message (item : int)]))
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  let handle =
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
           | "class" | "data-test" -> true
           | _ -> false)
         get_vdom)
      component
  in
  focus handle;
  keydown handle ArrowDown;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> a </div>
          <div class="focused-item"> c </div>
          <div> e </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.update items ~f:(fun map -> Map.set map ~key:2 ~data:"b");
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> b </div>
          <div class="focused-item"> c </div>
          <div> e </div>
        </div>
      </div>
    </div>
    |}];
  Bonsai.Expert.Var.update items ~f:(fun map -> Map.set map ~key:4 ~data:"d");
  Handle.show handle;
  [%expect
    {|
    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div> b </div>
          <div class="focused-item"> c </div>
          <div> d </div>
        </div>
      </div>
    </div>
    |}]
;;

module%test
  [@name "optimization: the query box only loads its data when interacted with"] _ =
struct
  let component =
    Bonsai_web_ui_query_box.create
      (module Int)
      ~max_visible_items:(Bonsai.return 3)
      ~f:(fun query (local_ _graph) ->
        let%arr query in
        print_endline "Generating options...";
        Map.filter items ~f:(String.is_prefix ~prefix:query) |> Map.map ~f:Node.text)
      ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
      ~on_select:(Bonsai.return (fun item -> Effect.print_s [%message (item : int)]))
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  ;;

  let%expect_test "focusing the input loads the data" =
    let handle = Handle.create (Result_spec.vdom get_vdom) component in
    Handle.show handle;
    [%expect
      {|
      <div>
        <input id="bonsai_path_replaced_in_test"
               type="text"
               #value=""
               @on_blur
               @on_focus
               @on_input
               @on_keydown/>
        <div data-test="query-box-item-container"
             id="bonsai_path_replaced_in_test"
             tabindex="-1"
             @on_blur
             @on_wheel
             style={
               position: relative;
             }>
          <div> </div>
        </div>
      </div>
      |}];
    focus handle;
    Handle.show_diff handle;
    [%expect
      {|
      Generating options...

        <div>
          <input id="bonsai_path_replaced_in_test"
                 type="text"
                 #value=""
                 @on_blur
                 @on_focus
                 @on_input
                 @on_keydown/>
          <div data-test="query-box-item-container"
               id="bonsai_path_replaced_in_test"
               tabindex="-1"
               @on_blur
               @on_wheel
               style={
                 position: relative;
               }>
      -|    <div> </div>
      +|    <div style={ position: absolute; }>
      +|      <div class="focused-item" @on_click @on_mouseenter> apple </div>
      +|      <div @on_click @on_mouseenter> orange </div>
      +|      <div @on_click @on_mouseenter> kiwi </div>
      +|    </div>
          </div>
        </div>
      |}]
  ;;

  let%expect_test "inputting text loads the data" =
    let handle = Handle.create (Result_spec.vdom get_vdom) component in
    Handle.show handle;
    [%expect
      {|
      <div>
        <input id="bonsai_path_replaced_in_test"
               type="text"
               #value=""
               @on_blur
               @on_focus
               @on_input
               @on_keydown/>
        <div data-test="query-box-item-container"
             id="bonsai_path_replaced_in_test"
             tabindex="-1"
             @on_blur
             @on_wheel
             style={
               position: relative;
             }>
          <div> </div>
        </div>
      </div>
      |}];
    input_text handle "some text";
    Handle.show_diff handle;
    [%expect
      {|
      Generating options...

        <div>
          <input id="bonsai_path_replaced_in_test"
                 type="text"
      -|         #value=""
      +|         #value="some text"
                 @on_blur
                 @on_focus
                 @on_input
                 @on_keydown/>
          <div data-test="query-box-item-container"
               id="bonsai_path_replaced_in_test"
               tabindex="-1"
               @on_blur
               @on_wheel
               style={
                 position: relative;
               }>
            <div> </div>
          </div>
        </div>
      |}]
  ;;

  let%expect_test "clicking a key loads the data" =
    let handle = Handle.create (Result_spec.vdom get_vdom) component in
    Handle.show handle;
    [%expect
      {|
      <div>
        <input id="bonsai_path_replaced_in_test"
               type="text"
               #value=""
               @on_blur
               @on_focus
               @on_input
               @on_keydown/>
        <div data-test="query-box-item-container"
             id="bonsai_path_replaced_in_test"
             tabindex="-1"
             @on_blur
             @on_wheel
             style={
               position: relative;
             }>
          <div> </div>
        </div>
      </div>
      |}];
    keydown handle ArrowDown;
    Handle.show_diff handle;
    [%expect
      {|
      ("default prevented" (key ArrowDown))
      Generating options...

        <div>
          <input id="bonsai_path_replaced_in_test"
                 type="text"
                 #value=""
                 @on_blur
                 @on_focus
                 @on_input
                 @on_keydown/>
          <div data-test="query-box-item-container"
               id="bonsai_path_replaced_in_test"
               tabindex="-1"
               @on_blur
               @on_wheel
               style={
                 position: relative;
               }>
      -|    <div> </div>
      +|    <div style={ position: absolute; }>
      +|      <div class="focused-item" @on_click @on_mouseenter> apple </div>
      +|      <div @on_click @on_mouseenter> orange </div>
      +|      <div @on_click @on_mouseenter> kiwi </div>
      +|    </div>
          </div>
        </div>
      |}]
  ;;
end

let%expect_test "[modify_input_on_select] field works" =
  let handle =
    let component (local_ graph) =
      Bonsai_web_ui_query_box.create
        (module Int)
        ~expand_direction:(Bonsai.return Bonsai_web_ui_query_box.Expand_direction.Down)
        ~max_visible_items:(Bonsai.return 3)
        ~f:(fun query (local_ _graph) ->
          let%arr query in
          Map.filter items ~f:(String.is_prefix ~prefix:query) |> Map.map ~f:Node.text)
        ~focused_item_attr:(Bonsai.return (Attr.class_ "focused-item"))
        ~on_select:(Bonsai.return (fun item -> Effect.print_s [%message (item : int)]))
        ~modify_input_on_select:(Bonsai.return (fun _ _ -> "oran"))
        ~on_hover_item:
          (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
        ()
        graph
    in
    Handle.create
      (Result_spec.vdom
         ~filter_printed_attributes:(fun ~key ~data:_ ->
           match key with
           | "class" | "data-test" | "value" -> true
           | _ -> false)
         get_vdom)
      component
  in
  focus handle;
  input_text handle "apple";
  Handle.show handle;
  [%expect
    {|
    <div>
      <input #value="apple"/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
        </div>
      </div>
    </div>
    |}];
  (* Clicking on "apple" selects it and sets the value of the input to "oran" *)
  Handle.click_on handle ~get_vdom ~selector:".focused-item";
  Handle.show_diff handle;
  [%expect
    {|
    (item 0)

      <div>
    -|  <input #value="apple"/>
    +|  <input #value="oran"/>
        <div data-test="query-box-item-container">
    -|    <div>
    -|      <div class="focused-item"> apple </div>
    -|    </div>
    +|    <div> </div>
        </div>
      </div>
    |}];
  (* Then, focusing the input shows completion suggestions that match "oran" *)
  focus handle;
  Handle.show_diff handle;
  [%expect
    {|
      <div>
        <input #value="oran"/>
        <div data-test="query-box-item-container">
    -|    <div> </div>
    +|    <div>
    +|      <div class="focused-item"> orange </div>
    +|    </div>
        </div>
      </div>
    |}]
;;

let bisimulate_both_focus
  ~(f :
      Bonsai_web_ui_query_box.On_focus.t
      -> expect_diff:
           (do_nothing:(unit -> unit) -> focus_first_element:(unit -> unit) -> unit)
      -> unit)
  =
  f Do_nothing ~expect_diff:(fun ~do_nothing ~focus_first_element:_ -> do_nothing ());
  f Focus_first_item ~expect_diff:(fun ~do_nothing:_ ~focus_first_element ->
    focus_first_element ())
;;

let%expect_test "Different on_focus_behavior" =
  bisimulate_both_focus ~f:(fun on_focus ~expect_diff ->
    let handle =
      create
        ~on_focus
        ~on_hover_item:
          (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
        ()
    in
    Handle.store_view handle;
    (* Focusing with [on_focus:Do_nothing] should not open the suggestion list *)
    (try focus handle with
     | _ -> print_endline "Attempted to focus, but nothing was focusable on-screen!");
    Handle.show handle;
    expect_diff
      ~do_nothing:(fun () ->
        [%expect
          {|
          Attempted to focus, but nothing was focusable on-screen!
          Focused item: None

          <div>
            <input/>
            <div data-test="query-box-item-container">
              <div> </div>
            </div>
          </div>
          |}])
      ~focus_first_element:(fun () ->
        [%expect
          {|
          Focused item: 0

          <div>
            <input/>
            <div data-test="query-box-item-container">
              <div>
                <div class="focused-item"> apple </div>
                <div> orange </div>
                <div> kiwi </div>
              </div>
            </div>
          </div>
          |}]);
    (* Pressing down should still open the suggestion list *)
    keydown handle ArrowDown;
    Handle.show_diff handle;
    expect_diff
      ~do_nothing:(fun () ->
        [%expect
          {|
          ("default prevented" (key ArrowDown))

          -|Focused item: None
          +|Focused item: 0

            <div>
              <input/>
              <div data-test="query-box-item-container">
          -|    <div> </div>
          +|    <div>
          +|      <div class="focused-item"> apple </div>
          +|      <div> orange </div>
          +|      <div> kiwi </div>
          +|    </div>
              </div>
            </div>
          |}])
      ~focus_first_element:(fun () ->
        [%expect
          {|
          ("default prevented" (key ArrowDown))

          -|Focused item: 0
          +|Focused item: 1

            <div>
              <input/>
              <div data-test="query-box-item-container">
                <div>
          -|      <div class="focused-item"> apple </div>
          -|      <div> orange </div>
          +|      <div> apple </div>
          +|      <div class="focused-item"> orange </div>
                  <div> kiwi </div>
                </div>
              </div>
            </div>
          |}]))
;;

let bisimulate_hover_behaviors
  ~(f :
      Bonsai_web_ui_query_box.On_hover_item.t
      -> expect_diff:
           (do_nothing:(unit -> unit) -> select_hovered_element:(unit -> unit) -> unit)
      -> unit)
  =
  f Do_nothing ~expect_diff:(fun ~do_nothing ~select_hovered_element:_ -> do_nothing ());
  f Focus_hovered_item ~expect_diff:(fun ~do_nothing:_ ~select_hovered_element ->
    select_hovered_element ())
;;

let%expect_test "different on_hover_item behaviors" =
  bisimulate_hover_behaviors ~f:(fun on_hover_item ~expect_diff ->
    let handle = create ~on_hover_item:(Bonsai.return on_hover_item) () in
    focus handle;
    Handle.show handle;
    [%expect
      {|
      Focused item: 0

      <div>
        <input/>
        <div data-test="query-box-item-container">
          <div>
            <div class="focused-item"> apple </div>
            <div> orange </div>
            <div> kiwi </div>
          </div>
        </div>
      </div>
      |}];
    (try
       Handle.mouseenter
         handle
         ~get_vdom
         ~selector:"[data-test=query-box-item-container] > div > div:nth-child(3)"
     with
     | _ ->
       print_endline
         "Attempted to mouse over an element, but that element is not hoverable!");
    Handle.show_diff handle;
    expect_diff
      ~select_hovered_element:(fun () ->
        (* Mousing over should select the item. *)
        [%expect
          {|
          -|Focused item: 0
          +|Focused item: 2

            <div>
              <input/>
              <div data-test="query-box-item-container">
                <div>
          -|      <div class="focused-item"> apple </div>
          +|      <div> apple </div>
                  <div> orange </div>
          -|      <div> kiwi </div>
          +|      <div class="focused-item"> kiwi </div>
                </div>
              </div>
            </div>
          |}])
      ~do_nothing:(fun () ->
        (* Mousing over should not select the item and there should be no diff. *)
        [%expect
          {| Attempted to mouse over an element, but that element is not hoverable! |}]))
;;

let%expect_test "inputting text and hitting enter immediately selects the correct item" =
  let handle =
    create
      ~on_hover_item:
        (Bonsai.return Bonsai_web_ui_query_box.On_hover_item.Focus_hovered_item)
      ()
  in
  keydown handle ArrowDown;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key ArrowDown))
    Focused item: 0

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div>
          <div class="focused-item"> apple </div>
          <div> orange </div>
          <div> kiwi </div>
        </div>
      </div>
    </div>
    |}];
  input_text handle "kiwi";
  keydown handle Enter;
  Handle.show handle;
  [%expect
    {|
    ("default prevented" (key Enter))
    (item 2)
    Focused item: None

    <div>
      <input/>
      <div data-test="query-box-item-container">
        <div> </div>
      </div>
    </div>
    |}]
;;
