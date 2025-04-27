open! Core
open! Bonsai_web
open! Async_kernel
open Jsdom
module Handle = Handle_experimental
open Bonsai_web_ui_toplayer
open Async_js_test

let () = Async_js.init ()

let tooltip_component ?(hoverable_inside = false) ?show_delay ?hide_grace_period _graph =
  let config =
    Tooltip.Config.create ~tooltip_attrs:[] ~arrow:None ?show_delay ?hide_grace_period ()
  in
  Bonsai.return
    {%html|
      <div id="container" %{Tooltip.text ~config ~hoverable_inside "tooltip"}>
        Hello World!
      </div>
    |}
;;

let filter_printed_attributes = Default_vdom_spec.Expert.filter_printed_attributes ()

let test component f =
  let async_time_source = Async_kernel.Time_source.create ~now:Time_ns.epoch () in
  let advance by = Async_kernel.Time_source.advance_by_alarms_by async_time_source by in
  Testable_timeout.For_running_tests.with_
    (Async_kernel.Time_source.read_only async_time_source)
    ~f:(fun () ->
      Handle.with_async
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        component
        (fun handle -> f (handle, advance)))
;;

let%expect_test "Tooltip opens on hover in, and closes on hover out" =
  let%bind.With handle, _ = test tooltip_component in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  Util.assert_not_in_dom handle;
  ignore ([%expect.output] : string);
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,13
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  (* Positioning happens asynchronously, but within a frame. *)
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -12,2 +12,3
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open">
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    |}];
  (* We can unhover the element, and the tooltip disappears. *)
  Handle.unhover handle;
  Handle.print_dom_diff handle;
  [%expect
    {|
    -1,19 +1,9
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open"
    -|         data-floating-placement="top">
    -|      <div> tooltip </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}];
  return ()
;;

let%expect_test "Moving mouse into hoverable outside tooltip keeps it around." =
  let%bind.With handle, advance = test (tooltip_component ~hoverable_inside:true) in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  Util.assert_not_in_dom handle;
  ignore ([%expect.output] : string);
  Handle.hover handle ~selector:"#container";
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,14
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  (* Doesn't close on leave, because there's a grace period  of 150ms. *)
  Handle.unhover handle;
  Handle.print_dom_diff handle;
  [%expect {| |}];
  let%bind () = advance (Time_ns.Span.of_ms 149.0) in
  Handle.hover handle ~selector:"[popover]";
  Handle.print_dom_diff handle;
  [%expect {| |}];
  Handle.unhover handle;
  Handle.print_dom_diff ~context:2 handle;
  [%expect {| |}];
  let%bind () = advance (Time_ns.Span.of_ms 149.0) in
  Handle.print_dom_diff ~context:2 handle;
  [%expect {| |}];
  (* Only closes after grace period has ended. Note that the grace period was reset when
     we hovered over the popover; otherwise, we would have closed in the previous print. *)
  let%bind () = advance (Time_ns.Span.of_ms 1.0) in
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,14 +6,4
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open"
    -|         data-floating-placement="top">
    -|      <div> tooltip </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}];
  return ()
;;

let%expect_test "Moving mouse out during the delay results in the tooltip not opening." =
  let%bind.With handle, advance =
    test (tooltip_component ~hoverable_inside:true ~show_delay:(Time_ns.Span.of_ms 100.0))
  in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  Util.assert_not_in_dom handle;
  ignore ([%expect.output] : string);
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff handle;
  [%expect {| |}];
  let%bind () = advance (Time_ns.Span.of_ms 99.0) in
  Handle.print_dom_diff handle;
  [%expect {| |}];
  Handle.unhover handle;
  (* Because the delay hasn't passed, the popover didn't open.
  This time, if we wait for 100 ms, it will open. *)
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff handle;
  (* Printing after 2ms confirms that the timer has reset, and a full 100ms are necessary. *)
  let%bind () = advance (Time_ns.Span.of_ms 2.0) in
  Handle.print_dom_diff handle;
  [%expect {| |}];
  let%bind () = advance (Time_ns.Span.of_ms 98.0) in
  (* This time it gets rendered into the DOM. Positioning happens asynchronously,
  but within the frame. *)
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff handle;
  [%expect
    {|
    -1,8 +1,19
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  return ()
;;

let%expect_test "If multiple tooltips are attached, only the last one works." =
  let%bind.With handle, _advance =
    test (fun _ ->
      Bonsai.return
        {%html|
          <div id="container" %{Tooltip.text "tooltip1"} %{Tooltip.text "tooltip2"}>
            Hello World!
          </div>
        |})
  in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  [%expect
    {|
    WARN: Multiple tooltips cannot be attached on the same element
    WARN: Multiple tooltips cannot be attached on the same element
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">  Hello World!  </div>
      </body>
    </html>
    |}];
  Handle.hover handle ~selector:"#container";
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">  Hello World!  </div>
      </body>
      <div>
        <div popover="auto"
             tabindex="-1"
             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
             mock-popover-state="open"
             data-floating-placement="top">
          <div> tooltip2 </div>
          <div style="display: contents"> </div>
        </div>
      </div>
    </html>
    |}];
  return ()
;;

let%expect_test "if mouse is removed between when the tooltip is rendered and when it \
                 opens, it will close: no grace period"
  =
  let%bind.With handle, _advance = test tooltip_component in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">  Hello World!  </div>
      </body>
    </html>
    |}];
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,13
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.unhover handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -8,10 +8,1
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    -|         mock-popover-state="open">
    -|      <div> tooltip </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
    |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect {| |}];
  return ()
;;

let%expect_test "if mouse is removed between when the tooltip is rendered and when it \
                 opens, it will close: no grace period"
  =
  let%bind.With handle, _advance = test tooltip_component in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">  Hello World!  </div>
      </body>
    </html>
    |}];
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,13
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.unhover handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -8,10 +8,1
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    -|         mock-popover-state="open">
    -|      <div> tooltip </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
    |}];
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect {| |}];
  return ()
;;

let%expect_test "if mouse is removed immediately after opening tooltip, it will close: \
                 with grace period"
  =
  let%bind.With handle, advance =
    test (tooltip_component ~hide_grace_period:Time_ns.Span.second)
  in
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">  Hello World!  </div>
      </body>
    </html>
    |}];
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,14
          <div tabindex="0" style="outline: none;">  Hello World!  </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    +|      <div> tooltip </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.unhover handle;
  Handle.print_dom_diff ~context:0 handle;
  (* Because there's a grace period, we don't close immediately. But when we go to open
     the tooltip, we'll check to see if it's hovered, see that it's not, and close it. *)
  [%expect {| |}];
  let%bind () = advance Time_ns.Span.second in
  (* Grace period. *)
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -8,11 +8,1
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open"
    -|         data-floating-placement="top">
    -|      <div> tooltip </div>
    -|      <div style="display: contents"> </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
    |}];
  return ()
;;

let%expect_test "Nested tooltips work" =
  let%bind.With handle, advance =
    test (fun _ ->
      let tooltip =
        Tooltip.create
          ~hoverable_inside:true
          (Vdom.Node.div
             ~attrs:
               [ Vdom.Attr.id "outer-tooltip"
               ; Tooltip.text ~hoverable_inside:true "inner tooltip"
               ]
             [ Vdom.Node.text "outer tooltip" ])
      in
      Bonsai.return {%html|<div id="container" %{tooltip}>Hello World!</div>|})
  in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"> </meta>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> Hello World! </div>
      </body>
    </html>
    |}];
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,15
          <div tabindex="0" style="outline: none;"> Hello World! </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         mock-popover-state="open">
    +|      <div>
    +|        <div> outer tooltip </div>
    +|      </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  (* Positioning happens asynchronously, but within the frame. *)
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -12,2 +12,3
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px;"
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open">
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    |}];
  Handle.unhover handle;
  let%bind () = advance (Time_ns.Span.of_ms 149.0) in
  Handle.hover handle ~selector:"#outer-tooltip";
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -16,5 +16,15
              <div> outer tooltip </div>
            </div>
    -|      <div style="display: contents"> </div>
    +|      <div style="display: contents">
    +|        <div popover="auto"
    +|             tabindex="-1"
    +|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    +|             mock-popover-state="open"
    +|             data-floating-placement="top">
    +|          <div> inner tooltip </div>
    +|          <div style="display: contents"> </div>
    +|        </div>
    +|      </div>
          </div>
        </div>
    |}];
  Handle.unhover handle;
  Handle.print_dom_diff handle;
  [%expect {| |}];
  let%bind () = advance (Time_ns.Span.of_ms 149.0) in
  Handle.print_dom_diff handle;
  [%expect {| |}];
  (* After the grace period, both tooltips disappear. *)
  let%bind () = advance (Time_ns.Span.of_ms 2.0) in
  Handle.print_dom_diff handle;
  [%expect
    {|
    -1,31 +1,9
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div tabindex="0" style="outline: none;"> Hello World! </div>
        </body>
    -|  <div>
    -|    <div popover="auto"
    -|         tabindex="-1"
    -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|         mock-popover-state="open"
    -|         data-floating-placement="top">
    -|      <div>
    -|        <div> outer tooltip </div>
    -|      </div>
    -|      <div style="display: contents">
    -|        <div popover="auto"
    -|             tabindex="-1"
    -|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    -|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    -|             mock-popover-state="open"
    -|             data-floating-placement="top">
    -|          <div> inner tooltip </div>
    -|          <div style="display: contents"> </div>
    -|        </div>
    -|      </div>
    -|    </div>
    -|  </div>
    +|  <div> </div>
      </html>
    |}];
  return ()
;;

let%expect_test "Tooltip content updates correctly" =
  let%bind.With handle, _advance =
    test (fun (local_ graph) ->
      let open Bonsai.Let_syntax in
      let count, set_count = Bonsai.state 0 graph in
      let incr_listener =
        let%arr count and set_count in
        Vdom.Attr.Global_listeners.keydown ~phase:Bubbling ~f:(fun _ ->
          set_count (count + 1))
      in
      let%arr incr_listener and count in
      {%html|
        <div id="container" %{incr_listener} %{Tooltip.text (Int.to_string count)}>
          %{count#Int}
        </div>
      |})
  in
  (* Initially we expect the tooltip to not be opened. *)
  Handle.print_dom handle;
  Util.assert_not_in_dom handle;
  ignore ([%expect.output] : string);
  (* We can hover the element, and the tooltip appears immediately. *)
  Handle.hover handle ~selector:"#container";
  let%bind () = Handle.bump_event_loop handle in
  Handle.print_dom_diff ~context:2 handle;
  [%expect
    {|
    -6,3 +6,14
          <div tabindex="0" style="outline: none;"> 0 </div>
        </body>
    +|  <div>
    +|    <div popover="auto"
    +|         tabindex="-1"
    +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
    +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); position: absolute; top: 0.00000000px; left: 0.00000000px; max-height: 0.00000000px; --floatingAvailableWidth: 0.00000000px;"
    +|         mock-popover-state="open"
    +|         data-floating-placement="top">
    +|      <div> 0 </div>
    +|      <div style="display: contents"> </div>
    +|    </div>
    +|  </div>
      </html>
    |}];
  Handle.press_key handle ~selector:"#container" ~code:KeyA;
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -6,1 +6,1
    -|    <div tabindex="0" style="outline: none;"> 0 </div>
    +|    <div tabindex="0" style="outline: none;"> 1 </div>
    -15,1 +15,1
    -|      <div> 0 </div>
    +|      <div> 1 </div>
    |}];
  Handle.press_key handle ~selector:"#container" ~code:KeyA;
  Handle.one_frame handle;
  Handle.print_dom_diff ~context:0 handle;
  [%expect
    {|
    -6,1 +6,1
    -|    <div tabindex="0" style="outline: none;"> 1 </div>
    +|    <div tabindex="0" style="outline: none;"> 2 </div>
    -15,1 +15,1
    -|      <div> 1 </div>
    +|      <div> 2 </div>
    |}];
  return ()
;;
