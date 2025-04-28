open! Core
open! Bonsai_web
open! Async_kernel
open Bonsai_web_ui_toplayer
open Jsdom
module Handle = Handle_experimental
open Util

let () = Async_js.init ()

let filter_printed_attributes =
  Default_vdom_spec.Expert.filter_printed_attributes ~show_id:true ()
;;

let%expect_test "Opening and closing flow: should close." =
  let f ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc ~action () =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun graph ->
        let%tydi { open_; _ } =
          Modal.create
            ~overflow_auto_wrapper:(return false)
            ?close_on_click_outside
            ?close_on_right_click_outside
            ?close_on_esc
            ~content:(fun ~close:_ _ -> Bonsai.return {%html|Modal|})
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
    assert_not_in_dom handle;
    Handle.click_on handle ~selector:"#anchor";
    (* Doesn't show up immediately, because it's driven by an on_change. *)
    assert_not_in_dom handle;
    Handle.one_frame handle;
    (* And one more frame for vdom hook's [on_mount] to run, and the modal to be opened. *)
    assert_open_and_shown handle;
    action handle;
    (* Once again, we need to wait for an [on_change] to drive the modal to close. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    assert_not_in_dom handle
  in
  f ~action:(fun handle -> Handle.click_on handle ~selector:"body") ();
  f
    ~close_on_right_click_outside:(Bonsai.return Close_on_click_outside.Yes)
    ~action:(fun handle -> Handle.right_click_on handle ~selector:"body")
    ();
  f ~action:(fun handle -> Handle.press_key handle ~selector:"body" ~code:Escape) ();
  f
    ~action:(fun handle ->
      Expect_test_helpers_core.require_does_raise (fun () ->
        Handle.click_on handle ~selector:"#anchor");
      [%expect
        {|
        ("Element matching query is inert; is there an open modal?"
         (selector #anchor)
         (here lib/bonsai/web_ui/toplayer/jsdom_test/test_modals.ml:59:8))
        |}];
      Handle.click_on handle ~selector:"body")
    ()
;;

let%expect_test "Opening and closing flow: should NOT close." =
  let f ?close_on_click_outside ?close_on_right_click_outside ?close_on_esc ~action () =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun graph ->
        let%tydi { open_; _ } =
          Modal.create
            ~overflow_auto_wrapper:(return false)
            ?close_on_click_outside
            ?close_on_right_click_outside
            ?close_on_esc
            ~content:(fun ~close:_ _ -> Bonsai.return {%html|Modal|})
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
    assert_not_in_dom handle;
    Handle.click_on handle ~selector:"#anchor";
    (* Doesn't show up immediately, because it's driven by an on_change. *)
    assert_not_in_dom handle;
    Handle.one_frame handle;
    (* And one more frame for vdom hook's [on_mount] to run, and the modal to be opened. *)
    assert_open_and_shown handle;
    action handle;
    (* Once again, we need to wait for an [on_change] to drive the modal. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    (* But this time, it stays open! *)
    assert_open_and_shown handle
  in
  f
    ~close_on_click_outside:(Bonsai.return Close_on_click_outside.No)
    ~action:(fun handle -> Handle.click_on handle ~selector:"body")
    ();
  f ~action:(fun handle -> Handle.right_click_on handle ~selector:"body") ();
  f
    ~close_on_esc:(Bonsai.return false)
    ~action:(fun handle -> Handle.press_key handle ~selector:"body" ~code:Escape)
    ();
  (* Mousedown inside, and mouseup + click outside shouldn't close. *)
  f
    ~action:(fun _handle ->
      Mouse_event.dispatch ~kind:Mouse_down ~selector:"[popover]" ();
      Mouse_event.dispatch ~kind:Mouse_up ~selector:"body" ();
      Mouse_event.dispatch ~kind:Click ~selector:"body" ())
    ()
;;

module%test Focus = struct
  let test
    ~document_starts_with_focus
    ~focus_input_before
    ~focus_on_open
    ~autofocus_in_modal
    ~focus_on_activate_in_modal
    ~print_on_load
    ~print_on_open
    ~print_on_close
    =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~filter_printed_attributes
        ~document_starts_with_focus
        ~get_vdom:Fn.id
        (fun graph ->
           let%tydi { open_; _ } =
             Modal.create
               ~overflow_auto_wrapper:(return false)
               ~focus_on_open:(Bonsai.return focus_on_open)
               ~content:(fun ~close:_ graph ->
                 let focus_on_activate =
                   if focus_on_activate_in_modal
                   then Effect.Focus.on_activate () graph
                   else return Vdom.Attr.empty
                 in
                 let autofocus =
                   if autofocus_in_modal
                   then Vdom.Attr.autofocus true
                   else Vdom.Attr.empty
                 in
                 let%arr focus_on_activate in
                 {%html|
                   <div>
                     Modal
                     <button %{focus_on_activate} %{autofocus}>Focusable</button>
                   </div>
                 |})
               graph
           in
           let%arr open_ in
           {%html|
             <div id="container">
               <input id="outside_input" />
               <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
             </div>
           |})
    in
    (* Initially we expect the modal to not be opened. *)
    assert_not_in_dom handle;
    if focus_input_before then Handle.focus handle ~selector:"#outside_input";
    Handle.print_active_element handle;
    print_on_load ();
    Handle.click_on handle ~selector:"#anchor";
    (* Doesn't show up immediately, because it's driven by an on_change. *)
    assert_not_in_dom handle;
    Handle.one_frame handle;
    (* And one more frame for vdom hook's [on_mount] to run, and the modal to be opened. *)
    assert_open_and_shown handle;
    Handle.print_active_element handle;
    print_on_open ();
    Handle.click_on handle ~selector:"body";
    (* Once again, we need to wait for an [on_change] to drive the modal. *)
    assert_open_and_shown handle;
    Handle.one_frame handle;
    (* And, it closes. *)
    assert_not_in_dom handle;
    Handle.print_active_element handle;
    print_on_close ()
  ;;

  let%expect_test "if modal does nothing with focus, opening and closing the modal \
                   doesn't change focus"
    =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;" inert=""> ... </div> |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "without autofocus / focus on activate, [focus_on_open] restores focus" =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}])
      ~print_on_open:(fun () ->
        [%expect
          {|
          <div popover="manual"
               tabindex="-1"
               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
               data-testing-modal=""
               id="bonsai_path_replaced_in_test"
               style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey;"
               mock-popover-state="open"> ... </div>
          |}])
      ~print_on_close:(fun () ->
        [%expect
          {| <div id="container" tabindex="0" style="outline: none;"> ... </div> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {|
          <div popover="manual"
               tabindex="-1"
               data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4=""
               data-testing-modal=""
               id="bonsai_path_replaced_in_test"
               style="--ppx_css_anonymous_var_1_hash_replaced_in_test: var(--floatingHeight, fit-content); --ppx_css_anonymous_var_2_hash_replaced_in_test: var(--floatingWidth, fit-content); --ppx_css_anonymous_var_3_hash_replaced_in_test: var(--floatingMinHeight); --ppx_css_anonymous_var_4_hash_replaced_in_test: var(--floatingMinWidth); --ppx_css_anonymous_var_5_hash_replaced_in_test: var(--floatingAvailableWidth, 100.00%); --ppx_css_anonymous_var_1_hash_replaced_in_test: white; --ppx_css_anonymous_var_2_hash_replaced_in_test: black; --ppx_css_anonymous_var_3_hash_replaced_in_test: 1px; --ppx_css_anonymous_var_4_hash_replaced_in_test: grey;"
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () -> [%expect {| <button autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
  ;;

  let%expect_test "focus on activate works without [focus_on_open]" =
    test
      ~document_starts_with_focus:true
      ~focus_input_before:false
      ~focus_on_open:false
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:true
      ~print_on_load:(fun () -> [%expect {| <input id="outside_input"> </input> |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test" autofocus=""> ... </button> |}])
      ~print_on_close:(fun () -> [%expect {| <input id="outside_input"> </input> |}]);
    test
      ~document_starts_with_focus:true
      ~focus_input_before:true
      ~focus_on_open:true
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:true
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
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect {| <button autofocus=""> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:false
      ~focus_on_open:true
      ~autofocus_in_modal:false
      ~focus_on_activate_in_modal:true
      ~print_on_load:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect
          {| <button data-focus-handle="bonsai_path_replaced_in_test"> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}]);
    test
      ~document_starts_with_focus:false
      ~focus_input_before:true
      ~focus_on_open:false
      ~autofocus_in_modal:true
      ~focus_on_activate_in_modal:false
      ~print_on_load:(fun () ->
        [%expect {| <input id="outside_input"> </input> (document unfocused) |}])
      ~print_on_open:(fun () ->
        [%expect {| <button autofocus=""> ... </button> (document unfocused) |}])
      ~print_on_close:(fun () -> [%expect {| <body> ... </body> (document unfocused) |}])
  ;;
end

let print_open_elements handle =
  let is_inert elem =
    Js_of_ocaml.(
      Js.Unsafe.meth_call elem "closest" [| Js.Unsafe.inject (Js.string "[inert]") |]
      |> Js.Opt.to_option
      |> Option.is_some)
  in
  (let root_inert = Handle.query_selector_exn handle ~selector:"#container" |> is_inert in
   if root_inert then print_endline "App root inert");
  Handle.query_selector_all handle ~selector:"[popover] span"
  |> List.map ~f:(fun elem ->
    let text = elem##.innerHTML |> Js_of_ocaml.Js.to_string in
    text ^ if is_inert elem then " (inert)" else "")
  |> String.concat_lines
  |> print_endline
;;

module%test [@name "multiple modals"] _ = struct
  let multiple_modals
    ?outer_close_on_click_outside
    ?inner_close_on_click_outside
    ?other_close_on_click_outside
    ?popover_close_on_click_outside
    graph
    =
    let open Bonsai.Let_syntax in
    let%tydi ( popover_attr
             , { open_ = popover_open; close = popover_close; is_open = popover_is_open }
             )
      =
      Popover.create
        ?close_on_click_outside:popover_close_on_click_outside
        ~overflow_auto_wrapper:(return false)
        ~content:(fun ~close:_ _ ->
          return
            {%html|<div><span>External Popover</span><button id="popover_button"></button></div>|})
        graph
    in
    let toggle_popover =
      let%arr popover_open and popover_close and popover_is_open in
      if popover_is_open then popover_close else popover_open
    in
    let%tydi { open_; _ } =
      Modal.create
        ~overflow_auto_wrapper:(return false)
        ?close_on_click_outside:outer_close_on_click_outside
        ~content:(fun ~close:close_outer graph ->
          let%tydi { open_ = open_inner; _ } =
            Modal.create
              ~overflow_auto_wrapper:(return false)
              ?close_on_click_outside:inner_close_on_click_outside
              ~content:(fun ~close:close_inner _ ->
                let%arr close_outer and close_inner and toggle_popover in
                {%html|
                  <div id="inner_modal">
                    <span> Inner Modal</span>
                    <button id="toggle_popover_from_inner" on_click=%{fun _ -> toggle_popover}>
                      Toggle Popover
                    </button>
                    <button id="close inner" on_click=%{fun _ -> close_inner}>
                      Close Outer
                    </button>
                    <button id="close_outer_from_inner" on_click=%{fun _ -> close_outer}>
                      Close Outer
                    </button>
                  </div>
                |})
              graph
          in
          let%arr close_outer and open_inner and toggle_popover in
          {%html|
            <div id="outer_modal">
              <span> Main Outer Modal</span>
              <button id="toggle_popover_from_outer" on_click=%{fun _ -> toggle_popover}>
                Toggle Popover
              </button>
              <button on_click=%{fun _ -> open_inner} id="open_inner">Open Inner</button>
              <button on_click=%{fun _ -> close_outer} id="close_outer">
                Close Outer
              </button>
            </div>
          |})
        graph
    in
    let%tydi { open_ = open_other; _ } =
      Modal.create
        ~overflow_auto_wrapper:(return false)
        ?close_on_click_outside:other_close_on_click_outside
        ~content:(fun ~close _ ->
          let%arr close and toggle_popover in
          {%html|
            <div id="other_modal">
              <span> Other Modal</span>
              <button id="toggle_popover_from_other" on_click=%{fun _ -> toggle_popover}>
                Toggle Popover
              </button>
              <button id="close_other" on_click=%{fun _ -> close}>Close</button>
            </div>
          |})
        graph
    in
    let%arr open_ and open_other and popover_attr and toggle_popover in
    {%html|
      <div id="container">
        <button
          id="open_popover"
          on_click=%{fun _ -> toggle_popover}
          %{popover_attr}
        >
          Toggle Popover
        </button>
        <button id="open_main" on_click=%{fun _ -> open_}>Open Main</button>
        <button id="open_other" on_click=%{fun _ -> open_other}>Open Other</button>
      </div>
    |}
  ;;

  let%expect_test "Nested modal state is independent of parent modal state." =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        (multiple_modals
           ~outer_close_on_click_outside:(return Close_on_click_outside.No)
           ~inner_close_on_click_outside:(return Close_on_click_outside.No)
           ~other_close_on_click_outside:(return Close_on_click_outside.No))
    in
    (* Initially we expect no modal to not be opened. *)
    print_open_elements handle;
    [%expect {| |}];
    Handle.click_on handle ~selector:"#open_main";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal
      |}];
    Handle.click_on handle ~selector:"#open_inner";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Inner Modal
      |}];
    (* Can't interact with inert parent.*)
    Expect_test_helpers_core.require_does_raise (fun () ->
      Handle.click_on handle ~selector:"#close_outer");
    [%expect
      {|
      ("Element matching query is inert; is there an open modal?"
       (selector #close_outer)
       (here lib/bonsai/web_ui/toplayer/jsdom_test/test_modals.ml:637:6))
      |}];
    (* Closing outer implicitly closes inner. *)
    Handle.click_on handle ~selector:"#close_outer_from_inner";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect {| |}];
    (* When outer is re-opened, inner re-opens with it. *)
    Handle.click_on handle ~selector:"#open_main";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Inner Modal
      |}]
  ;;

  let%expect_test "Inertness interactions between multiple modals and popovers. " =
    let%bind.With handle =
      let open Bonsai.Let_syntax in
      Handle.with_
        ~filter_printed_attributes
        ~get_vdom:Fn.id
        (multiple_modals
           ~outer_close_on_click_outside:(return Close_on_click_outside.Yes)
           ~inner_close_on_click_outside:(return Close_on_click_outside.Yes)
           ~other_close_on_click_outside:
             (return Close_on_click_outside.Yes_unless_target_is_popover)
           ~popover_close_on_click_outside:(return Close_on_click_outside.No))
    in
    (* Initially we expect no popover to not be opened. *)
    print_open_elements handle;
    [%expect {| |}];
    (* We can open these 2 back-to-back, but after a frame, both will open, and then
       we'll only be able to interact with [other]. *)
    Handle.click_on handle ~selector:"#open_main";
    Handle.click_on handle ~selector:"#open_other";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Other Modal
      |}];
    Handle.click_on handle ~selector:"#toggle_popover_from_other";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Other Modal
      External Popover
      |}];
    (* Clicking on the popover doesn't close the "main" one even though it's close behavior
       is [Yes] not [Yes_unless_target_is_popover], because inert elements don't get closed
       on click outside. It doesn't close the "other" modal because its behavior is
       [Yes_unless_target_is_popover]. *)
    Handle.click_on handle ~selector:"#popover_button";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Other Modal
      External Popover
      |}];
    (* Closing the "other" modal keeps the popover open. *)
    Handle.click_on handle ~selector:"#close_other";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal
      External Popover
      |}];
    (* This time, clicking on the popover _does_ close the main modal, because it is no
       longer inert. *)
    Handle.click_on handle ~selector:"#popover_button";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect {| External Popover |}];
    (* Reopening the main modal makes the popover inert *)
    Handle.click_on handle ~selector:"#open_main";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
      External Popover (inert)
       Main Outer Modal
      |}];
    (* Opening inner modal makes the popover inert. *)
    Handle.click_on handle ~selector:"#open_inner";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
      External Popover (inert)
       Main Outer Modal (inert)
       Inner Modal
      |}];
    (* We can close and re-open the popover, and it'll end up on top + not inert. *)
    Handle.click_on handle ~selector:"#toggle_popover_from_inner";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Inner Modal
      |}];
    Handle.click_on handle ~selector:"#toggle_popover_from_inner";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal (inert)
       Inner Modal
      External Popover
      |}];
    (* Clicking outside closes the inner modal. *)
    Handle.click_on handle ~selector:"body";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect
      {|
      App root inert
       Main Outer Modal
      External Popover
      |}];
    (* One final time, and both modals are closed. *)
    Handle.click_on handle ~selector:"body";
    Handle.one_frame handle;
    print_open_elements handle;
    [%expect {| External Popover |}]
  ;;
end

let%expect_test "Inertness works when the app root element is changed while opening the \
                 modal"
  =
  let%bind.With handle =
    let open Bonsai.Let_syntax in
    Handle.with_ ~filter_printed_attributes ~get_vdom:Fn.id (fun graph ->
      let%tydi { open_; is_open; _ } =
        Modal.create
          ~overflow_auto_wrapper:(return false)
          ~content:(fun ~close:_ _ -> Bonsai.return {%html|<span>Modal</span>|})
          graph
      in
      let%arr open_ and is_open in
      match is_open with
      | true ->
        {%html|
          <div id="container">
            <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
          </div>
        |}
      | false ->
        {%html|
          <main id="container">
            <button id="anchor" on_click=%{fun _ -> open_}>Open</button>
          </main>
        |})
  in
  (* Initially we expect the modal to not be opened. *)
  print_open_elements handle;
  [%expect {| |}];
  Handle.click_on handle ~selector:"#anchor";
  (* Doesn't show up immediately, because it's driven by an on_change. *)
  print_open_elements handle;
  [%expect {| |}];
  Handle.one_frame handle;
  (* And one more frame for vdom hook's [on_mount] to run, and the modal to be opened. *)
  print_open_elements handle;
  [%expect
    {|
    App root inert
    Modal
    |}];
  Handle.click_on handle ~selector:"body";
  (* Once again, we need to wait for an [on_change] to drive the modal to close. *)
  print_open_elements handle;
  [%expect
    {|
    App root inert
    Modal
    |}];
  Handle.one_frame handle;
  print_open_elements handle;
  [%expect {| |}]
;;
