open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Js_of_ocaml
open Jsdom
module Handle = Handle_experimental

let with_logging f =
  Bonsai_web_ui_codemirror.For_testing.enable_logging ();
  protect ~f ~finally:(fun () -> Bonsai_web_ui_codemirror.For_testing.disable_logging ())
;;

let get_dom_string selector =
  let nodes =
    Dom_html.document##querySelectorAll (Js.string selector)
    |> Js_of_ocaml__Dom.list_of_nodeList
  in
  match nodes with
  | [] -> [ [%string "No matching node found for selector \"%{selector}\""] ]
  | nodes ->
    List.map nodes ~f:(fun node ->
      Jsdom.Expert_for_custom_test_handles.Dom_serialization.dom_to_string ~node ())
;;

let setup_mock_codemirror_methods () =
  let open Js_of_ocaml in
  let f =
    Js.Unsafe.js_expr
      {js|
    (() => {

function getBoundingClientRect() {
    const rec = {
        x: 0,
        y: 0,
        bottom: 0,
        height: 0,
        left: 0,
        right: 0,
        top: 0,
        width: 0,
    };
    return {...rec, toJSON: () => rec};
}

class FakeDOMRectList extends Array {
    item(index) {
        return this[index];
    }
}

document.elementFromPoint = () => null;
globalThis.window.HTMLElement.prototype.getBoundingClientRect = getBoundingClientRect;
globalThis.window.HTMLElement.prototype.getClientRects = () => new FakeDOMRectList();
globalThis.window.Range.prototype.getBoundingClientRect = getBoundingClientRect;
globalThis.window.Range.prototype.getClientRects = () => new FakeDOMRectList();

    })
    |js}
  in
  (Js.Unsafe.fun_call f [||] : unit)
;;

module Expect_test_config = struct
  include Expect_test_config

  let run f =
    run (fun () ->
      setup_mock_codemirror_methods ();
      f ())
  ;;
end

let get_vdom codemirror =
  let node = Bonsai_web_ui_codemirror.view codemirror in
  Vdom.Node.div
    [ Vdom.Node.div ~attrs:[ Vdom.Attr.id "one" ] [ node ]
    ; Vdom.Node.div ~attrs:[ Vdom.Attr.id "two" ] [ node ]
    ]
;;

let set_lines handle lines =
  Bonsai_web_ui_codemirror.send_transaction
    handle
    (Bonsai_web_ui_codemirror.Transaction.set_lines lines)
;;

let set_selection ~anchor ?head handle =
  Bonsai_web_ui_codemirror.send_transaction handle (fun state ->
    Codemirror.State.Editor_state.update
      state
      [ Codemirror.State.Transaction_spec.create
          ~selection:(Codemirror.State.Editor_selection.single ~anchor ?head ())
          ()
      ])
;;

(** By default, this checks the contents of each codemirror wrapper div against the other,
    as the wrappers themselves will never be equal *)
let print_for_both ?(selector = "> *") () =
  let one = get_dom_string [%string "#one %{selector}"] in
  let two = get_dom_string [%string "#two %{selector}"] in
  match List.equal String.equal one two with
  | true -> List.iter ~f:print_endline one
  | false ->
    Expect_test_helpers_base.print_cr
      [%message "Expected contents of both codemirror instances to match but they don't"];
    print_endline "ONE:";
    List.iter ~f:print_endline one;
    print_endline "TWO:";
    List.iter ~f:print_endline two
;;

let%expect_test "Test to make sure updates are correctly propagated between instances of \
                 the same exact vdom node"
  =
  let%bind.With handle =
    Handle.with_ ~get_vdom (fun graph ->
      let extensions =
        [ Codemirror.Basic_setup.basic_setup, "basic"
        ; Codemirror_themes.get Basic_dark, "dark_theme"
        ]
      in
      Bonsai_web_ui_codemirror.of_initial_state
        ~name:"editor"
        (Codemirror.State.Editor_state.create
           (Codemirror.State.Editor_state_config.create
              ~extensions:(extensions |> List.map ~f:fst)
              ()))
        graph)
  in
  Handle.inject handle (fun handle -> set_lines handle [ "abc"; "def" ]);
  Handle.one_frame handle;
  print_for_both ();
  (* This is just making sure that both instances of the vdom node print and that they show
  the same thing *)
  [%expect
    {|
    <div class="cm-editor ͼo ͼq ͼr ͼ1b">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div class="cm-gutters" aria-hidden="true" style="min-height: 28px; position: sticky;">
          <div class="cm-gutter cm-lineNumbers">
            <div class="cm-gutterElement"
                 style="height: 0px; visibility: hidden; pointer-events: none;"> 9 </div>
            <div class="cm-gutterElement cm-activeLineGutter" style="height: 14px;"> 1 </div>
            <div class="cm-gutterElement" style="height: 14px;"> 2 </div>
          </div>
          <div class="cm-gutter cm-foldGutter">
            <div class="cm-gutterElement"
                 style="height: 0px; visibility: hidden; pointer-events: none;">
              <span title="Unfold line"> › </span>
            </div>
            <div class="cm-gutterElement cm-activeLineGutter" style="height: 14px;"> </div>
          </div>
        </div>
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-activeLine cm-line"> abc </div>
          <div class="cm-line"> def </div>
        </div>
        <div class="cm-layer cm-layer-above cm-cursorLayer"
             aria-hidden="true"
             style="z-index: 150; animation-duration: 1200ms;"> </div>
        <div class="cm-layer cm-selectionLayer" aria-hidden="true" style="z-index: -2;"> </div>
      </div>
    </div>
    |}];
  print_for_both ~selector:".cm-activeLine" ();
  [%expect {| <div class="cm-activeLine cm-line"> abc </div> |}];
  with_logging (fun () ->
    Handle.inject handle (fun handle -> set_selection handle ~anchor:0 ~head:6);
    Handle.one_frame handle;
    (* This should print something like "From 0 to 1" to show that updates are actually
     propagating to both instances of the widget. Please make sure to check that
     it is not cycling updates *)
    [%expect {| From widget id 0 updating other widget id 1 |}]);
  print_for_both ~selector:".cm-activeLine" ();
  (* Should have updated to def for both *)
  [%expect {| <div class="cm-activeLine cm-line"> def </div> |}]
;;

let%expect_test "Test to make sure that dynamic_extension (non-prime) initializes with \
                 the extensions enabled. It should happen on the second frame."
  =
  let%bind.With handle =
    Handle.with_ ~get_vdom (fun graph ->
      let extensions = [ Codemirror_sexp.Rainbow_parentheses.extension () ] in
      Bonsai_web_ui_codemirror.with_dynamic_extensions
        ~basic_setup:`Basic
        ~equal:(fun _ _ -> false)
        ~compute_extensions:(Bonsai.return (fun () -> extensions))
        ~name:"editor"
        ~initial_state:
          (Codemirror.State.Editor_state.create
             (Codemirror.State.Editor_state_config.create
                ~doc:"((one two))"
                ~extensions:[]
                ()))
        (Bonsai.return ())
        graph)
  in
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ͼr">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div class="cm-gutters" aria-hidden="true" style="min-height: 14px; position: sticky;">
          <div class="cm-gutter cm-lineNumbers">
            <div class="cm-gutterElement"
                 style="height: 0px; visibility: hidden; pointer-events: none;"> 9 </div>
            <div class="cm-gutterElement cm-activeLineGutter" style="height: 14px;"> 1 </div>
          </div>
          <div class="cm-gutter cm-foldGutter">
            <div class="cm-gutterElement"
                 style="height: 0px; visibility: hidden; pointer-events: none;">
              <span title="Unfold line"> › </span>
            </div>
            <div class="cm-gutterElement cm-activeLineGutter" style="height: 14px;"> </div>
          </div>
        </div>
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-activeLine cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
        <div class="cm-layer cm-layer-above cm-cursorLayer"
             aria-hidden="true"
             style="z-index: 150; animation-duration: 1200ms;"> </div>
        <div class="cm-layer cm-selectionLayer" aria-hidden="true" style="z-index: -2;"> </div>
      </div>
    </div>
    |}]
;;

let%expect_test "Test to make sure dynamic_extensions' updates the extensions" =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _) -> get_vdom codemirror)
      (fun graph ->
        let extensions, set_extensions = Bonsai.state [] graph in
        let codemirror =
          Bonsai_web_ui_codemirror.with_dynamic_extensions'
            ~name:"editor"
            ~extensions
            ~initial_text:"((one two))"
            graph
        in
        let%arr set_extensions and codemirror in
        codemirror, set_extensions)
  in
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, set_extensions) ->
    set_extensions [ Codemirror_sexp.Rainbow_parentheses.extension () ]);
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "Test to make sure that dynamic_extension' initializes with the \
                 extensions enabled"
  =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _) -> get_vdom codemirror)
      (fun graph ->
        let extensions, set_extensions =
          Bonsai.state [ Codemirror_sexp.Rainbow_parentheses.extension () ] graph
        in
        let codemirror =
          Bonsai_web_ui_codemirror.with_dynamic_extensions'
            ~name:"editor"
            ~extensions
            ~initial_text:"((one two))"
            graph
        in
        let%arr set_extensions and codemirror in
        codemirror, set_extensions)
  in
  print_for_both ();
  let first_frame = [%expect.output] in
  Handle.one_frame handle;
  print_for_both ();
  let second_frame = [%expect.output] in
  Expect_test_helpers_base.require_equal (module String) first_frame second_frame;
  print_endline first_frame;
  {%expect|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}
;;

let%expect_test "Test scope model sends transactions to the proper codemirror instance" =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _, _) -> get_vdom codemirror)
      (fun graph ->
        let scope, toggle_scope = Bonsai.toggle ~default_model:true graph in
        let codemirror =
          Bonsai.scope_model
            (module Bool)
            ~on:scope
            ~for_:(fun graph ->
              let extensions, set_extensions = Bonsai.state [] graph in
              let codemirror =
                Bonsai_web_ui_codemirror.with_dynamic_extensions'
                  ~name:"editor"
                  ~extensions
                  ~initial_text:"((one two))"
                  graph
              in
              Bonsai.both codemirror set_extensions)
            graph
        in
        let%arr codemirror, set_extensions = codemirror
        and scope
        and toggle_scope in
        codemirror, set_extensions, (scope, toggle_scope))
  in
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, set_extensions, _) ->
    set_extensions [ Codemirror_sexp.Rainbow_parentheses.extension () ]);
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, _, (_, toggle_scope)) -> toggle_scope);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, _, (_, toggle_scope)) -> toggle_scope);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "Check to make sure deactivated nodes can still have transactions sent \
                 to them"
  =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _) -> get_vdom codemirror)
      (fun graph ->
        let scope, set_scope = Bonsai.state `A graph in
        let gen_instance graph =
          Bonsai_web_ui_codemirror.with_dynamic_extensions'
            ~name:"editor"
            ~extensions:(Bonsai.return [])
            ~initial_text:"((one two))"
            graph
        in
        let active_instance =
          match%sub scope with
          | `A -> gen_instance graph
          | `B -> gen_instance graph
        in
        let%arr active_instance and scope and set_scope in
        active_instance, (scope, set_scope))
  in
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  (* Retrieve the last computation for `A before switching *)
  let codemirror_a, _ = Handle.last_result handle in
  (* Checks to make sure that we can still update the state properly after the node has
     been unmounted/deactivated *)
  Handle.inject handle (fun (_, (_, set_scope)) -> set_scope `B);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun _ -> set_lines codemirror_a [ "abc"; "def" ]);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, (_, set_scope)) -> set_scope `A);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> abc </div>
          <div class="cm-line"> def </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "Re-activated nodes can receive transactions properly" =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _, (_, _)) -> get_vdom codemirror)
      (fun graph ->
        let extensions, set_extensions = Bonsai.state [] graph in
        let scope, toggle_scope = Bonsai.toggle ~default_model:true graph in
        let codemirror =
          match%sub scope with
          | true ->
            Bonsai_web_ui_codemirror.with_dynamic_extensions'
              ~name:"editor"
              ~extensions
              ~initial_text:"((one two))"
              graph
          | false ->
            Bonsai_web_ui_codemirror.with_dynamic_extensions'
              ~name:"editor"
              ~extensions:(Bonsai.return [])
              ~initial_text:"STATIC"
              graph
        in
        let%arr set_extensions and codemirror and scope and toggle_scope in
        codemirror, set_extensions, (scope, toggle_scope))
  in
  Handle.inject handle (fun (_, set_extensions, _) ->
    set_extensions [ Codemirror_sexp.Rainbow_parentheses.extension () ]);
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, _, (_, toggle_scope)) -> toggle_scope);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> STATIC </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, _, (_, toggle_scope)) -> toggle_scope);
  (* [reset] takes more than one animation frame. This is because [Path_and_generation]
     uses [on_change] which queues up another change after display, which takes effect
     the next animation frame since [run_animation_frame] makes a copy of all the events
     in the queue before running any of them *)
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <span class="rainbow-colors-0_hash_replaced_in_test"> ( </span>
            <span class="rainbow-colors-1_hash_replaced_in_test"> ( </span>
            one two
            <span class="rainbow-colors-1_hash_replaced_in_test"> ) </span>
            <span class="rainbow-colors-0_hash_replaced_in_test"> ) </span>
          </div>
        </div>
      </div>
    </div>
    |}];
  (* Checks to make sure that we can still update the state properly after the node has
     been unmounted/deactivated *)
  Handle.inject handle (fun (handle, _, _) -> set_lines handle [ "abc"; "def" ]);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> abc </div>
          <div class="cm-line"> def </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "with_model_resetter actually resets the model" =
  let%bind.With handle =
    Handle.with_
      ~get_vdom:(fun (codemirror, _) -> get_vdom codemirror)
      (fun graph ->
        let codemirror, reset =
          Bonsai.with_model_resetter
            ~f:
              (Bonsai_web_ui_codemirror.with_dynamic_extensions'
                 ~name:"editor"
                 ~extensions:(Bonsai.return [])
                 ~initial_text:"((one two))")
            graph
        in
        let%arr codemirror and reset in
        codemirror, reset)
  in
  (* Check to make sure we can update after model has been reset *)
  Handle.inject handle (fun (handle, _) -> set_lines handle [ "12345"; "67890" ]);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> 12345 </div>
          <div class="cm-line"> 67890 </div>
        </div>
      </div>
    </div>
    |}];
  Handle.inject handle (fun (_, reset) -> reset);
  (* [reset] takes more than one animation frame. This is because [Path_and_generation]
     uses [on_change] which queues up another change after display, which takes effect
     the next animation frame since [run_animation_frame] makes a copy of all the events
     in the queue before running any of them *)
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> ((one two)) </div>
        </div>
      </div>
    </div>
    |}];
  (* Check to make sure we can update after model has been reset *)
  Handle.inject handle (fun (handle, _) -> set_lines handle [ "abc"; "def" ]);
  (* [reset] takes more than one animation frame. This is because [Path_and_generation]
     uses [on_change] which queues up another change after display, which takes effect
     the next animation frame since [run_animation_frame] makes a copy of all the events
     in the queue before running any of them *)
  Handle.one_frame handle;
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> abc </div>
          <div class="cm-line"> def </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "demonstrate that transactions can be sent to unrendered cm instances" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:Tuple3.get1 (fun graph ->
      let extensions = [] in
      let editor_1 =
        Bonsai_web_ui_codemirror.of_initial_state
          ~name:"editor"
          (Codemirror.State.Editor_state.create
             (Codemirror.State.Editor_state_config.create
                ~extensions:(extensions |> List.map ~f:fst)
                ()))
          graph
      in
      let editor_2 =
        Bonsai_web_ui_codemirror.of_initial_state
          ~name:"editor"
          (Codemirror.State.Editor_state.create
             (Codemirror.State.Editor_state_config.create
                ~extensions:(extensions |> List.map ~f:fst)
                ()))
          graph
      in
      let which, set_which = Bonsai.state `First graph in
      let%arr editor_1 and editor_2 and which and set_which in
      let view =
        Bonsai_web_ui_codemirror.view
          (match which with
           | `First -> editor_1
           | `Second -> editor_2)
      in
      ( Vdom.Node.div ~attrs:[ Vdom.Attr.id "root" ] [ view ]
      , (editor_1, editor_2)
      , set_which ))
  in
  (* Set the on-screen code-mirror content to "ABC" *)
  Handle.inject handle (fun (_view, (cm1, _cm2), _set_which) -> set_lines cm1 [ "ABC" ]);
  Handle.one_frame handle;
  List.iter (get_dom_string "#root") ~f:print_endline;
  [%expect
    {|
    <div id="root" tabindex="0" style="outline: none;">
      <div class="cm-editor ͼo ͼp ">
        <div class="cm-announced" aria-live="polite"> </div>
        <div tabindex="-1" class="cm-scroller">
          <div style="tab-size: 4;"
               spellcheck="false"
               autocorrect="off"
               autocapitalize="off"
               translate="no"
               contenteditable="true"
               class="cm-content"
               role="textbox"
               aria-multiline="true">
            <div class="cm-line"> ABC </div>
          </div>
        </div>
      </div>
    </div>
    |}];
  (* Set the _off_-screen code-mirror content to "XYZ" *)
  Handle.inject handle (fun (_view, (_cm1, cm2), _set_which) -> set_lines cm2 [ "XYZ" ]);
  Handle.one_frame handle;
  (* Flip to the 2nd instance *)
  Handle.inject handle (fun (_view, (_cm1, _cm2), set_which) -> set_which `Second);
  Handle.one_frame handle;
  List.iter (get_dom_string "#root") ~f:print_endline;
  [%expect
    {|
    <div id="root" tabindex="0" style="outline: none;">
      <div class="cm-editor ͼo ͼp ">
        <div class="cm-announced" aria-live="polite"> </div>
        <div tabindex="-1" class="cm-scroller">
          <div style="tab-size: 4;"
               spellcheck="false"
               autocorrect="off"
               autocapitalize="off"
               translate="no"
               contenteditable="true"
               class="cm-content"
               role="textbox"
               aria-multiline="true">
            <div class="cm-line"> XYZ </div>
          </div>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "Setting lines to an empty array" =
  let%bind.With handle =
    Handle.with_ ~get_vdom (fun graph ->
      Bonsai_web_ui_codemirror.with_dynamic_extensions'
        ~name:"editor"
        ~extensions:(Bonsai.return [])
        ~initial_text:"abc"
        graph)
  in
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line"> abc </div>
        </div>
      </div>
    </div>
    |}];
  (* Check to make sure we can update after model has been reset *)
  Handle.inject handle (fun handle -> set_lines handle []);
  Handle.one_frame handle;
  print_for_both ();
  [%expect
    {|
    <div class="cm-editor ͼo ͼp ">
      <div class="cm-announced" aria-live="polite"> </div>
      <div tabindex="-1" class="cm-scroller">
        <div style="tab-size: 4;"
             spellcheck="false"
             autocorrect="off"
             autocapitalize="off"
             translate="no"
             contenteditable="true"
             class="cm-content"
             role="textbox"
             aria-multiline="true">
          <div class="cm-line">
            <br/>
          </div>
        </div>
      </div>
    </div>
    |}]
;;
