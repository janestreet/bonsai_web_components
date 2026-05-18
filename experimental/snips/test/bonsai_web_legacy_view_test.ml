open! Core
open! Bonsai_web
open! Bonsai_web_test
module Snips = Bonsai_experimental_snips
open Snips.Infix

let run_test' layout =
  let handle =
    Bonsai_web_test.Handle.create (Result_spec.vdom Fn.id) (fun (local_ _graph) ->
      Bonsai.return (Snips.render layout))
  in
  Handle.show handle;
  let out = Expect_test_helpers_base.expect_test_output () in
  let out = String.substr_replace_all out ~pattern:"_hash_replaced_in_test" ~with_:"" in
  print_endline out
;;

let run_test layout =
  run_test' (layout |+| Snips.body ~attr:(Vdom.Attr.class_ "body") (Vdom.Node.text "body"))
;;

let text = Vdom.Node.text
let class_ = Vdom.Attr.class_

let top () =
  Snips.top ~attr:(class_ "top") (Vdom.Node.none_deprecated [@alert "-deprecated"])
;;

let bottom () =
  Snips.bottom ~attr:(class_ "bottom") (Vdom.Node.none_deprecated [@alert "-deprecated"])
;;

let left () =
  Snips.left ~attr:(class_ "left") (Vdom.Node.none_deprecated [@alert "-deprecated"])
;;

let right () =
  Snips.right ~attr:(class_ "right") (Vdom.Node.none_deprecated [@alert "-deprecated"])
;;

let body s =
  Snips.body ~attr:(Vdom.Attr.class_ s) (Vdom.Node.none_deprecated [@alert "-deprecated"])
;;

let%expect_test "only body" =
  Snips.body ~attr:(class_ "body") (text "body") |> run_test';
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr;
           grid-template-rows: 1fr;
         }>
      <div class="body" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top" =
  run_test (top ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr;
           grid-template-rows: auto 1fr;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "bottom" =
  run_test (bottom ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr;
           grid-template-rows: 1fr auto;
         }>
      <div class="bottom"
           style={
             grid-area: 2 / 1 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="body" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "left" =
  run_test (left ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: auto 1fr;
           grid-template-rows: 1fr;
         }>
      <div class="left"
           style={
             grid-area: 1 / 1 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 1 / 2 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "right" =
  run_test (right ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr auto;
           grid-template-rows: 1fr;
         }>
      <div class="right"
           style={
             grid-area: 1 / 2 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + bottom" =
  run_test (top () |+| bottom ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr;
           grid-template-rows: auto 1fr auto;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="bottom"
           style={
             grid-area: 3 / 1 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "left + right" =
  run_test (left () |+| right ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: auto 1fr auto;
           grid-template-rows: 1fr;
         }>
      <div class="left"
           style={
             grid-area: 1 / 1 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="right"
           style={
             grid-area: 1 / 3 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 1 / 2 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + left" =
  run_test (top () |+| left ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: auto 1fr;
           grid-template-rows: auto 1fr;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="left"
           style={
             grid-area: 2 / 1 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 2 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + right" =
  run_test (top () |+| right ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr auto;
           grid-template-rows: auto 1fr;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="right"
           style={
             grid-area: 2 / 2 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + left + bottom" =
  run_test (top () |+| left () |+| bottom ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: auto 1fr;
           grid-template-rows: auto 1fr auto;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="left"
           style={
             grid-area: 2 / 1 / span 2 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="bottom"
           style={
             grid-area: 3 / 2 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 2 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + right + bottom" =
  run_test (top () |+| right () |+| bottom ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr auto;
           grid-template-rows: auto 1fr auto;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="right"
           style={
             grid-area: 2 / 2 / span 2 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="bottom"
           style={
             grid-area: 3 / 1 / span 1 / span 1;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "top + bottom + right" =
  run_test (top () |+| bottom () |+| right ());
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr auto;
           grid-template-rows: auto 1fr auto;
         }>
      <div class="top"
           style={
             grid-area: 1 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="bottom"
           style={
             grid-area: 3 / 1 / span 1 / span 2;
             overflow-x: auto;
             overflow-y: hidden;
           }> </div>
      <div class="right"
           style={
             grid-area: 2 / 2 / span 1 / span 1;
             overflow-y: auto;
             overflow-x: hidden;
           }> </div>
      <div class="body" style={ grid-area: 2 / 1 / span 1 / span 1; overflow: auto; }> body </div>
    </div>
    |}]
;;

let%expect_test "split_h" =
  run_test' (Snips.split_h [ body "a"; body "b" ]);
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr 1fr;
           grid-template-rows: 1fr;
         }>
      <div class="root"
           style={
             display: grid;
             grid-template-columns: 1fr;
             grid-template-rows: 1fr;
             grid-area: 1 / 1 / span 1 / span 1;
           }>
        <div class="a" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> </div>
      </div>
      <div class="root"
           style={
             display: grid;
             grid-template-columns: 1fr;
             grid-template-rows: 1fr;
             grid-area: 1 / 2 / span 1 / span 1;
           }>
        <div class="b" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> </div>
      </div>
    </div>
    |}]
;;

let%expect_test "split_v" =
  run_test' (Snips.split_v [ body "a"; body "b" ]);
  [%expect
    {|
    <div class="root"
         style={
           display: grid;
           grid-template-columns: 1fr;
           grid-template-rows: 1fr 1fr;
         }>
      <div class="root"
           style={
             display: grid;
             grid-template-columns: 1fr;
             grid-template-rows: 1fr;
             grid-area: 1 / 1 / span 1 / span 1;
           }>
        <div class="a" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> </div>
      </div>
      <div class="root"
           style={
             display: grid;
             grid-template-columns: 1fr;
             grid-template-rows: 1fr;
             grid-area: 2 / 1 / span 1 / span 1;
           }>
        <div class="b" style={ grid-area: 1 / 1 / span 1 / span 1; overflow: auto; }> </div>
      </div>
    </div>
    |}]
;;
