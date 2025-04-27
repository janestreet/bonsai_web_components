open! Core
open! Bonsai_web
open Bonsai_web_ui_toplayer
open Jsdom
module Handle = Handle_experimental
open Util

let filter_printed_attributes =
  Default_vdom_spec.Expert.filter_printed_attributes ~show_id:true ()
;;

let%expect_test "Opening and closing flow: should close." =
  let f ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc ~action () =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
        let%tydi attr, { open_; _ } =
          Popover.create
            ~overflow_auto_wrapper:(return false)
            ?close_on_click_outside
            ?close_on_right_click_outside
            ?close_on_esc
            ~content:(fun ~close:_ _ -> Bonsai.return {%html|Popover|})
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
    assert_not_in_dom handle;
    Handle.click_on handle ~selector:"#anchor";
    (* Schedules a state change on the action queue. *)
    assert_not_in_dom handle;
    Handle.one_frame handle;
    (* Opens the next frame. *)
    assert_open_and_shown handle;
    action handle;
    (* Once again, we need to wait for an [on_change] to drive the popover to close. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    assert_not_in_dom handle
  in
  f ~action:(fun handle -> Handle.click_on handle ~selector:"#container") ();
  f
    ~close_on_right_click_outside:(Bonsai.return Close_on_click_outside.Yes)
    ~action:(fun handle -> Handle.right_click_on handle ~selector:"#container")
    ();
  f ~action:(fun handle -> Handle.press_key handle ~selector:"#container" ~code:Escape) ();
  (* Clicking on anchor while popover is open should close it. *)
  f ~action:(fun handle -> Handle.click_on handle ~selector:"#anchor") ()
;;

let%expect_test "Opening and closing flow: should NOT close." =
  let f ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc ~action () =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
        let%tydi attr, { open_; _ } =
          Popover.create
            ?close_on_click_outside
            ?close_on_right_click_outside
            ?close_on_esc
            ~overflow_auto_wrapper:(return false)
            ~content:(fun ~close:_ _ -> Bonsai.return {%html|Popover|})
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
    assert_not_in_dom handle;
    Handle.click_on handle ~selector:"#anchor";
    (* Schedules a state change on the action queue. *)
    assert_not_in_dom handle;
    (* Opens the next frame. *)
    Handle.one_frame handle;
    assert_open_and_shown handle;
    action handle;
    (* Once again, we need to wait for an [on_change] to drive the popover. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    (* But this time, it stays open! *)
    assert_open_and_shown handle
  in
  f
    ~close_on_click_outside:(Bonsai.return Close_on_click_outside.No)
    ~action:(fun handle -> Handle.click_on handle ~selector:"#container")
    ();
  f ~action:(fun handle -> Handle.right_click_on handle ~selector:"#container") ();
  f
    ~close_on_esc:(Bonsai.return false)
    ~action:(fun handle -> Handle.press_key handle ~selector:"#container" ~code:Escape)
    ();
  (* We want to ensure that clicking on the anchor doesn't re-open the popover.*)
  f ~action:(fun handle -> Handle.right_click_on handle ~selector:"#anchor") ();
  f
    ~close_on_esc:(Bonsai.return false)
    ~action:(fun handle -> Handle.press_key handle ~selector:"#anchor" ~code:Escape)
    ();
  (* Mousedown inside, and mouseup + click outside shouldn't close. *)
  f
    ~action:(fun _handle ->
      Mouse_event.dispatch ~kind:Mouse_down ~selector:"[popover]" ();
      Mouse_event.dispatch ~kind:Mouse_up ~selector:"#container" ();
      Mouse_event.dispatch ~kind:Click ~selector:"#container" ())
    ()
;;

let%expect_test "Closes if you click on the anchor, even if anchor has [Stop_propagation]"
  =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun (local_ graph) ->
      let%tydi attr, { open_; _ } =
        Popover.create
          ~overflow_auto_wrapper:(return false)
          ~content:(fun ~close:_ _ -> Bonsai.return {%html|Popover|})
          graph
      in
      let%arr attr and open_ in
      let on_click _ = Effect.Many [ open_; Effect.Stop_propagation ] in
      {%html|
        <div id="container">
          <button id="anchor" %{attr} on_click=%{on_click}>Open</button>
        </div>
      |})
  in
  Handle.click_on handle ~selector:"#anchor";
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
      <div>
        <div popover="manual"
             tabindex="-1"
             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
             id="bonsai_path_replaced_in_test"
             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
             mock-popover-state="open">
          <div> Popover </div>
          <div style="display: contents"> </div>
        </div>
      </div>
    </html>
    |}];
  Handle.click_on handle ~selector:"#anchor";
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
    |}]
;;

module%test Focus = struct
  let test
    ~document_starts_with_focus
    ~focus_input_before
    ~focus_on_open
    ~autofocus_in_popover
    ~focus_on_activate_in_popover
    ~print_on_load
    ~print_on_open
    ~print_on_close
    =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~document_starts_with_focus
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        (fun (local_ graph) ->
           let%tydi attr, { open_; _ } =
             Popover.create
               ~overflow_auto_wrapper:(return false)
               ~focus_on_open:(Bonsai.return focus_on_open)
               ~content:(fun ~close:_ (local_ graph) ->
                 let focus_on_activate =
                   if focus_on_activate_in_popover
                   then Effect.Focus.on_activate () graph
                   else return Vdom.Attr.empty
                 in
                 let autofocus =
                   if autofocus_in_popover
                   then Vdom.Attr.autofocus true
                   else Vdom.Attr.empty
                 in
                 let%arr focus_on_activate in
                 {%html|
                   <div>
                     Popover
                     <button %{focus_on_activate} %{autofocus}>Focusable</button>
                   </div>
                 |})
               graph
           in
           let%arr attr and open_ in
           {%html|
             <div id="container">
               <input id="outside_input" />
               <button id="anchor" %{attr} on_click=%{fun _ -> open_}>Open</button>
             </div>
           |})
    in
    (* Initially we expect the popover to not be opened. *)
    assert_not_in_dom handle;
    if focus_input_before then Handle.focus handle ~selector:"#outside_input";
    Handle.print_active_element handle;
    print_on_load ();
    Handle.click_on handle ~selector:"#anchor";
    (* Doesn't show up immediately, because it's driven by an on_change. *)
    assert_not_in_dom handle;
    Handle.one_frame handle;
    (* And one more frame for vdom hook's [on_mount] to run, and the popover to be opened. *)
    assert_open_and_shown handle;
    Handle.print_active_element handle;
    print_on_open ();
    Handle.click_on handle ~selector:"#container";
    (* Once again, we need to wait for an [on_change] to drive the popover. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    (* And, it closes. *)
    assert_not_in_dom handle;
    Handle.print_active_element handle;
    print_on_close ()
  ;;

  let%expect_test "if popover does nothing with focus, opening and closing the popover \
                   doesn't change focus"
    =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "without autofocus / focus on activate, [focus_on_open] restores focus" =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {|
          <div popover="manual"
               tabindex="-1"
               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
               id="bonsai_path_replaced_in_test"
               style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
               mock-popover-state="open"> ... </div>
          |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {|
          <div popover="manual"
               tabindex="-1"
               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
               id="bonsai_path_replaced_in_test"
               style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
               mock-popover-state="open"> ... </div>
          |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "If [autofocus] and [focus_on_open], autofocus wins, and focus is \
                   restored on close"
    =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () -> [%expect {| <button autofocus=""> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () -> [%expect {| <button autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "If [autofocus] but not [focus_on_open], focus is still restored on \
                   close"
    =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () -> [%expect {| <button autofocus=""> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () -> [%expect {| <button autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "focus on activate works without [focus_on_open]" =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "focus on activate works with [focus_on_open]" =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "Combo of [autofocus] and [focus_on_activate] results in focus still \
                   being applied."
    =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test" autofocus=""> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test" autofocus=""> ... </button> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test" autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test" autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "If the document doesn't have focus, toplayer won't attempt to focus \
                   elements."
    =
    test
      ~document_starts_with_focus:false
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect {| <button autofocus=""> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_popover:false
      ~focus_on_activate_in_popover:true
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_popover:true
      ~focus_on_activate_in_popover:false
      ~print_on_load:(fun () ->
        [%expect {| <input id="outside_input"> </input> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect {| <button autofocus=""> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
  ;;
end

module%test [@name "multiple popovers"] _ = struct
  let multiple_popovers
    ?outer_close_on_click_outside
    ?inner_close_on_click_outside
    ?other_close_on_click_outside
    (local_ graph)
    =
    let open Bonsai.Let_syntax in
    let%tydi attr, { open_; _ } =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ?close_on_click_outside:outer_close_on_click_outside
        ~content:(fun ~close (local_ graph) ->
          let%tydi inner_attr, { open_ = open_inner; _ } =
            Popover.create
              ~overflow_auto_wrapper:(return false)
              ?close_on_click_outside:inner_close_on_click_outside
              ~content:(fun ~close _ ->
                let%arr close in
                {%html|
                  <div id="inner_popover">
                    Inner Popover
                    <button id="close_inner" on_click=%{fun _ -> close}>Close</button>
                  </div>
                |})
              graph
          in
          let%arr close and inner_attr and open_inner in
          {%html|
            <div id="outer_popover">
              Outer Popover
              <button %{inner_attr} on_click=%{fun _ -> open_inner} id="open_inner">
                Open Inner
              </button>
              <button id="close_outer" on_click=%{fun _ -> close}>Open Inner</button>
            </div>
          |})
        graph
    in
    let%tydi other_attr, { open_ = open_other; _ } =
      Popover.create
        ~overflow_auto_wrapper:(return false)
        ?close_on_click_outside:other_close_on_click_outside
        ~content:(fun ~close _ ->
          let%arr close in
          {%html|
            <div id="other_popover">
              Other Popover
              <button id="close_other" on_click=%{fun _ -> close}>Close</button>
            </div>
          |})
        graph
    in
    let%arr attr and other_attr and open_ and open_other in
    {%html|
      <div id="container">
        <button id="open_main" %{attr} on_click=%{fun _ -> open_}>
          Open Main
        </button>
        <button id="open_other" %{other_attr} on_click=%{fun _ -> open_other}>
          Open Other
        </button>
      </div>
    |}
  ;;

  let%expect_test "Nested popover state is independent of parent popover state." =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        (multiple_popovers
           ~outer_close_on_click_outside:(return Close_on_click_outside.No)
           ~inner_close_on_click_outside:(return Close_on_click_outside.No)
           ~other_close_on_click_outside:(return Close_on_click_outside.No))
    in
    (* Initially we expect no popover to not be opened. *)
    Handle.print_dom handle;
    [%expect
      {|
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div id="container" tabindex="0" style="outline: none;">
            <button id="open_main">  Open Main  </button>
            <button id="open_other">  Open Other  </button>
          </div>
        </body>
        <div> </div>
      </html>
      |}];
    Handle.click_on handle ~selector:"#open_main";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -7,6 +7,22
              <button id="open_main">  Open Main  </button>
              <button id="open_other">  Open Other  </button>
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
      +|        <div id="outer_popover">
      +|           Outer Popover
      +|          <button id="open_inner">  Open Inner  </button>
      +|          <button id="close_outer"> Open Inner </button>
      +|        </div>
      +|      </div>
      +|      <div style="display: contents"> </div>
      +|    </div>
      +|  </div>
        </html>
      |}];
    Handle.click_on handle ~selector:"#open_inner";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -21,8 +21,23
                  <button id="open_inner">  Open Inner  </button>
                  <button id="close_outer"> Open Inner </button>
                </div>
              </div>
      -|      <div style="display: contents"> </div>
      +|      <div style="display: contents">
      +|        <div popover="manual"
      +|             tabindex="-1"
      +|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|             id="bonsai_path_replaced_in_test"
      +|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      +|             mock-popover-state="open">
      +|          <div>
      +|            <div id="inner_popover">
      +|               Inner Popover
      +|              <button id="close_inner"> Close </button>
      +|            </div>
      +|          </div>
      +|          <div style="display: contents"> </div>
      +|        </div>
      +|      </div>
            </div>
          </div>
        </html>
      |}];
    (* Closing outer closes both inner and outer. *)
    Handle.click_on handle ~selector:"#close_outer";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -7,37 +7,6
              <button id="open_main">  Open Main  </button>
              <button id="open_other">  Open Other  </button>
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
      -|        <div id="outer_popover">
      -|           Outer Popover
      -|          <button id="open_inner">  Open Inner  </button>
      -|          <button id="close_outer"> Open Inner </button>
      -|        </div>
      -|      </div>
      -|      <div style="display: contents">
      -|        <div popover="manual"
      -|             tabindex="-1"
      -|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      -|             id="bonsai_path_replaced_in_test"
      -|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      -|             mock-popover-state="open">
      -|          <div>
      -|            <div id="inner_popover">
      -|               Inner Popover
      -|              <button id="close_inner"> Close </button>
      -|            </div>
      -|          </div>
      -|          <div style="display: contents"> </div>
      -|        </div>
      -|      </div>
      -|    </div>
      -|  </div>
      +|  <div> </div>
        </html>
      |}];
    (* When outer is re-opened, inner re-opens with it. *)
    Handle.click_on handle ~selector:"#open_main";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -7,6 +7,37
              <button id="open_main">  Open Main  </button>
              <button id="open_other">  Open Other  </button>
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
      +|        <div id="outer_popover">
      +|           Outer Popover
      +|          <button id="open_inner">  Open Inner  </button>
      +|          <button id="close_outer"> Open Inner </button>
      +|        </div>
      +|      </div>
      +|      <div style="display: contents">
      +|        <div popover="manual"
      +|             tabindex="-1"
      +|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|             id="bonsai_path_replaced_in_test"
      +|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      +|             mock-popover-state="open">
      +|          <div>
      +|            <div id="inner_popover">
      +|               Inner Popover
      +|              <button id="close_inner"> Close </button>
      +|            </div>
      +|          </div>
      +|          <div style="display: contents"> </div>
      +|        </div>
      +|      </div>
      +|    </div>
      +|  </div>
        </html>
      |}]
  ;;

  let%expect_test "On click outside with `Yes_unless_target_is_popover` works" =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        (multiple_popovers
           ~outer_close_on_click_outside:(return Close_on_click_outside.Yes)
           ~inner_close_on_click_outside:(return Close_on_click_outside.Yes)
           ~other_close_on_click_outside:
             (return Close_on_click_outside.Yes_unless_target_is_popover))
    in
    (* Initially we expect no popover to not be opened. *)
    Handle.print_dom handle;
    [%expect
      {|
      <html>
        <head>
          <meta charset="UTF-8"> </meta>
        </head>
        <body>
          <div id="container" tabindex="0" style="outline: none;">
            <button id="open_main">  Open Main  </button>
            <button id="open_other">  Open Other  </button>
          </div>
        </body>
        <div> </div>
      </html>
      |}];
    Handle.click_on handle ~selector:"#open_main";
    Handle.click_on handle ~selector:"#open_other";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -7,6 +7,36
              <button id="open_main">  Open Main  </button>
              <button id="open_other">  Open Other  </button>
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
      +|        <div id="outer_popover">
      +|           Outer Popover
      +|          <button id="open_inner">  Open Inner  </button>
      +|          <button id="close_outer"> Open Inner </button>
      +|        </div>
      +|      </div>
      +|      <div style="display: contents"> </div>
      +|    </div>
      +|    <div popover="manual"
      +|         tabindex="-1"
      +|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|         id="bonsai_path_replaced_in_test"
      +|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      +|         mock-popover-state="open">
      +|      <div>
      +|        <div id="other_popover">
      +|           Other Popover
      +|          <button id="close_other"> Close </button>
      +|        </div>
      +|      </div>
      +|      <div style="display: contents"> </div>
      +|    </div>
      +|  </div>
        </html>
      |}];
    Handle.click_on handle ~selector:"#open_inner";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -21,9 +21,24
                  <button id="open_inner">  Open Inner  </button>
                  <button id="close_outer"> Open Inner </button>
                </div>
              </div>
      -|      <div style="display: contents"> </div>
      +|      <div style="display: contents">
      +|        <div popover="manual"
      +|             tabindex="-1"
      +|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      +|             id="bonsai_path_replaced_in_test"
      +|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      +|             mock-popover-state="open">
      +|          <div>
      +|            <div id="inner_popover">
      +|               Inner Popover
      +|              <button id="close_inner"> Close </button>
      +|            </div>
      +|          </div>
      +|          <div style="display: contents"> </div>
      +|        </div>
      +|      </div>
            </div>
            <div popover="manual"
                 tabindex="-1"
                 data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      |}];
    (* Now, we've opened everything.
       Clicking on the outer one should close just the inner one. *)
    Handle.click_on handle ~selector:"#outer_popover";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -21,24 +21,9
                  <button id="open_inner">  Open Inner  </button>
                  <button id="close_outer"> Open Inner </button>
                </div>
              </div>
      -|      <div style="display: contents">
      -|        <div popover="manual"
      -|             tabindex="-1"
      -|             data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      -|             id="bonsai_path_replaced_in_test"
      -|             style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      -|             mock-popover-state="open">
      -|          <div>
      -|            <div id="inner_popover">
      -|               Inner Popover
      -|              <button id="close_inner"> Close </button>
      -|            </div>
      -|          </div>
      -|          <div style="display: contents"> </div>
      -|        </div>
      -|      </div>
      +|      <div style="display: contents"> </div>
            </div>
            <div popover="manual"
                 tabindex="-1"
                 data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      |}];
    (* Now, we've opened everything.
       Clicking on the other one should close the "main" outer one. *)
    Handle.click_on handle ~selector:"#other_popover";
    Handle.one_frame handle;
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -8,23 +8,8
              <button id="open_other">  Open Other  </button>
            </div>
          </body>
          <div>
      -|    <div popover="manual"
      -|         tabindex="-1"
      -|         data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
      -|         id="bonsai_path_replaced_in_test"
      -|         style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey; position: absolute; top: 0.00000000px; left: 0.00000000px;"
      -|         mock-popover-state="open">
      -|      <div>
      -|        <div id="outer_popover">
      -|           Outer Popover
      -|          <button id="open_inner">  Open Inner  </button>
      -|          <button id="close_outer"> Open Inner </button>
      -|        </div>
      -|      </div>
      -|      <div style="display: contents"> </div>
      -|    </div>
            <div popover="manual"
                 tabindex="-1"
                 data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
                 id="bonsai_path_replaced_in_test"
      |}];
    (* And finally, clicking on the container should close the "other" one. *)
    Handle.click_on handle ~selector:"#container";
    Handle.one_frame handle;
    Handle.print_dom_diff ~context:4 handle;
    [%expect
      {|
      -7,21 +7,6
              <button id="open_main">  Open Main  </button>
              <button id="open_other">  Open Other  </button>
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
      -|        <div id="other_popover">
      -|           Other Popover
      -|          <button id="close_other"> Close </button>
      -|        </div>
      -|      </div>
      -|      <div style="display: contents"> </div>
      -|    </div>
      -|  </div>
      +|  <div> </div>
        </html>
      |}]
  ;;
end
