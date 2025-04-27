open! Core
open! Bonsai_web
open Bonsai_web_ui_toplayer
open Jsdom
module Handle = Handle_experimental

(* The other tests try to be quite general, and test many scenarios. But that makes them
   annoying to use for debugging the specific `Focus.on_activate` issue, so we also have
   these specific ones. *)

let filter_printed_attributes =
  Default_vdom_spec.Expert.filter_printed_attributes ~show_id:true ()
;;

let%expect_test "Popover" =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
      let%tydi attr, { open_; _ } =
        Popover.create
          ~overflow_auto_wrapper:(return false)
          ~content:(fun ~close:_ graph ->
            let%arr focus_on_activate =
              Effect.Focus.on_activate ~name_for_testing:"popover" () graph
            in
            {%html|<button %{focus_on_activate}>Focusable</button>|})
          graph
      in
      let%arr attr and open_ in
      {%html|
        <div id="container">
          <button id="anchor" %{attr} on_click=%{fun _ -> open_}>Open</button>
        </div>
      |})
  in
  (* Initially we expect the popover to not be opened. *)
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div id="container" tabindex="0" style="outline: none;">
          <button id="anchor"> Open </button>
        </div>
      </body>
      <div> </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
  (* At this point, we've enqueued an action to toggle the popover state, but nothing has
     changed.*)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.print_active_element handle;
  [%expect {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}];
  (* In this frame, the a hook on the anchor is initialized,
     and [Effect.Focus.on_activate] runs *)
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,6 +6,18
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div> </div>
    +|  <div>
    +|    <div popover="manual"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         id="bonsai_path_replaced_in_test"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div>
    +|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.print_active_element handle;
  [%expect {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}];
  Handle.click_on handle ~selector:"#container";
  (* An action to update state is enqueued. *)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,18 +6,6
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div>
    -|    <div popover="manual"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         id="bonsai_path_replaced_in_test"
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
    -|         mock-popover-state="open">
    -|      <div>
    -|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    -|      </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}]
;;

let%expect_test "Virtual Popover" =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
      let%tydi { open_; _ } =
        Popover.create_virtual
          ~overflow_auto_wrapper:(return false)
          ~content:(fun ~close:_ graph ->
            let%arr focus_on_activate =
              Effect.Focus.on_activate ~name_for_testing:"virtual-popover" () graph
            in
            {%html|<button %{focus_on_activate}>Focusable</button>|})
          (return (Anchor.of_coordinate ~relative_to:`Viewport ~x:0.0 ~y:0.0))
          graph
      in
      let%arr open_ in
      {%html|
        <div id="container">
          <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
        </div>
      |})
  in
  (* Initially we expect the popover to not be opened. *)
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div id="container" tabindex="0" style="outline: none;">
          <button id="anchor"> Open </button>
        </div>
      </body>
      <div> </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
  (* At this point, we've enqueued an action to toggle the popover state, but nothing has
     changed.*)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  (* In this frame, [on_activate] runs and we attempt to focus the nonexistent popover.
     Then, [on_change] runs, and the popover is created. *)
  Handle.print_active_element handle;
  [%expect {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,6 +6,18
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div> </div>
    +|  <div>
    +|    <div popover="manual"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         id="bonsai_path_replaced_in_test"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: fixed; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div>
    +|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.print_active_element handle;
  [%expect {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}];
  Handle.click_on handle ~selector:"#container";
  (* Once again, we need to wait for an [on_change] to drive the popover to close. *)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,18 +6,6
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div>
    -|    <div popover="manual"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         id="bonsai_path_replaced_in_test"
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: fixed; top: 0.00000000px; left: 0.00000000px;"
    -|         mock-popover-state="open">
    -|      <div>
    -|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    -|      </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}]
;;

let%expect_test "CSS Popover" =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
      let%tydi { open_; _ } =
        Popover.create_css (* Not actually positioned in tests. *)
          ~extra_attrs:(return [ Vdom.Attr.create "css_positioned" "got applied!" ])
          ~content:(fun ~close:_ graph ->
            let%arr focus_on_activate =
              Effect.Focus.on_activate ~name_for_testing:"css-popover" () graph
            in
            {%html|<button %{focus_on_activate}>Focusable</button>|})
          graph
      in
      let%arr open_ in
      {%html|
        <div id="container">
          <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
        </div>
      |})
  in
  (* Initially we expect the popover to not be opened. *)
  Handle.one_frame handle;
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div id="container" tabindex="0" style="outline: none;">
          <button id="anchor"> Open </button>
        </div>
      </body>
      <div> </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
  (* At this point, we've enqueued an action to toggle the popover state, but nothing has
     changed.*)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  (* In this frame, [on_activate] runs and we attempt to focus the nonexistent popover.
     Then, [on_change] runs, and the popover is created. *)
  Handle.print_active_element handle;
  [%expect {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,6 +6,19
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div> </div>
    +|  <div>
    +|    <div popover="manual"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         id="bonsai_path_replaced_in_test"
    +|         css_positioned="got applied!"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%);"
    +|         mock-popover-state="open">
    +|      <div>
    +|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.print_active_element handle;
  [%expect {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}];
  Handle.click_on handle ~selector:"#container";
  (* Once again, we need to wait for an [on_change] to drive the popover to close. *)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -6,19 +6,6
          <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div>
    -|    <div popover="manual"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         id="bonsai_path_replaced_in_test"
    -|         css_positioned="got applied!"
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%);"
    -|         mock-popover-state="open">
    -|      <div>
    -|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    -|      </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}]
;;

let%expect_test "Modal" =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
      let%tydi { open_; _ } =
        Modal.create
          ~overflow_auto_wrapper:(return false)
          ~content:(fun ~close:_ graph ->
            let%arr focus_on_activate =
              Effect.Focus.on_activate ~name_for_testing:"modal" () graph
            in
            {%html|<button %{focus_on_activate}>Focusable</button>|})
          graph
      in
      let%arr open_ in
      {%html|
        <div id="container">
          <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
        </div>
      |})
  in
  (* Initially we expect the modal to not be opened. *)
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div id="container" tabindex="0" style="outline: none;">
          <button id="anchor"> Open </button>
        </div>
      </body>
      <div> </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
  (* At this point, we've enqueued an action to toggle the modal state, but nothing has
     changed.*)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.print_active_element handle;
  [%expect {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}];
  (* In this frame, [on_activate] runs and we attempt to focus the modal.
     Then, [on_change] runs, and the modal is created. *)
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -2,10 +2,23
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
    -|    <div id="container" tabindex="0" style="outline: none;">
    +|    <div id="container" tabindex="0" style="outline: none;" inert="">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div> </div>
    +|  <div>
    +|    <div popover="manual"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         data-testing-modal=""
    +|         id="bonsai_path_replaced_in_test"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey;"
    +|         mock-popover-state="open">
    +|      <div>
    +|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.print_active_element handle;
  [%expect {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}];
  Handle.click_on handle ~selector:"body";
  (* Once again, we need to wait for an [on_change] to drive the popover to close. *)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -2,23 +2,10
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
    -|    <div id="container" tabindex="0" style="outline: none;" inert="">
    +|    <div id="container" tabindex="0" style="outline: none;">
            <button id="anchor"> Open </button>
          </div>
        </body>
    -|  <div>
    -|    <div popover="manual"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         data-testing-modal=""
    -|         id="bonsai_path_replaced_in_test"
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey;"
    -|         mock-popover-state="open">
    -|      <div>
    -|        <button data-focus-handle="bonsai_path_replaced_in_test"> Focusable </button>
    -|      </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}]
;;
