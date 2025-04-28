open! Core
open! Bonsai_web
open Bonsai_web_ui_toplayer
open Jsdom
module Handle = Handle_experimental

let filter_printed_attributes =
  Default_vdom_spec.Expert.filter_printed_attributes ~show_id:true ()
;;

let%expect_test "moving anchor causes popover to close and re-open, losing focus" =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun graph ->
      let%tydi attr, { open_; _ } =
        Popover.create
          ~overflow_auto_wrapper:(return false)
          ~close_on_click_outside:(return Close_on_click_outside.No)
          ~content:(fun ~close:_ graph ->
            let%arr focus_on_activate =
              Effect.Focus.on_activate ~name_for_testing:"popover" () graph
            in
            {%html|<button %{focus_on_activate} id="popover_button">Focusable</button>|})
          graph
      in
      let has_prev_sibling, toggle_has_prev_sibling =
        Bonsai.toggle ~default_model:false graph
      in
      let%arr attr and open_ and has_prev_sibling and toggle_has_prev_sibling in
      let prev_sibling =
        if has_prev_sibling
        then Vdom.Node.text "hi"
        else Vdom.Node.none_deprecated [@alert "-deprecated"]
      in
      {%html|
        <div id="container">
          %{prev_sibling}
          <button id="anchor" %{attr} on_click=%{fun _ -> open_}>Open</button>
          <button
            id="prev_sibling_toggle"
            on_click=%{fun _ -> toggle_has_prev_sibling}
          >
            Toggle Prev Sibling
          </button>
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
          <button id="prev_sibling_toggle">  Toggle Prev Sibling  </button>
        </div>
      </body>
      <div> </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
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
    -7,6 +7,18
            <button id="anchor"> Open </button>
            <button id="prev_sibling_toggle">  Toggle Prev Sibling  </button>
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
    +|        <button data-focus-handle="bonsai_path_replaced_in_test" id="popover_button"> Focusable </button>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.print_active_element handle;
  [%expect
    {| <button data-focus-handle="bonsai_path_replaced_in_test" id="popover_button"> ... </button> |}];
  Handle.click_on handle ~selector:"#prev_sibling_toggle";
  Handle.print_active_element handle;
  [%expect
    {| <button data-focus-handle="bonsai_path_replaced_in_test" id="popover_button"> ... </button> |}];
  (* Restore focus to the popover before it closes. *)
  Handle.click_on handle ~selector:"#popover_button";
  Handle.print_active_element handle;
  [%expect
    {| <button data-focus-handle="bonsai_path_replaced_in_test" id="popover_button"> ... </button> |}];
  (* An action to update state is enqueued. *)
  Handle.print_dom_diff ~context:4 handle;
  [%expect {| |}];
  (* Inserting a previous sibling causes vdom to re-render the list, so [destroy] is called,
     followed by a new [on_mount]. Within the same frame, the popover is re-opened, as it was. *)
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:4 handle;
  [%expect
    {|
    -3,8 +3,9
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div id="container" tabindex="0" style="outline: none;">
    +|      hi
            <button id="anchor"> Open </button>
            <button id="prev_sibling_toggle">  Toggle Prev Sibling  </button>
          </div>
        </body>
    |}];
  (* Sadly, focus is lost from the popover. *)
  Handle.print_active_element handle;
  [%expect {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]
;;
