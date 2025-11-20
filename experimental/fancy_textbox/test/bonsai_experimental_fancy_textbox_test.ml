open! Core
open! Bonsai_web
open! Bonsai_web_test
module Fancy = Bonsai_experimental_fancy_textbox

let run_test ~text ~process () =
  let vdom = Fancy.create ~text ~set_text:(fun _ -> Effect.return ()) ~process () in
  vdom
  |> Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn
  |> Virtual_dom_test_helpers.Node_helpers.select_first ~selector:"pre"
  |> Option.value_exn
  |> Virtual_dom_test_helpers.Node_helpers.to_string_html
       ~filter_printed_attributes:(fun ~key ~data:_ -> not (String.equal key "class"))
  |> print_endline
;;

let%expect_test "fancy textbox with no highlighting" =
  run_test ~text:"foo bar" ~process:(fun s -> [ s, Fancy.Decoration.create () ]) ();
  [%expect
    {|
    <pre>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span> foo bar </span>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </pre>
    |}]
;;

let%expect_test "multi-line string, no highlighting" =
  (* Of note are the "spacer" <span>s, and the fact that the whitepsace is represented
     literally. *)
  run_test
    ~text:"\n\nfoo\nbar\n\n"
    ~process:(fun s -> [ s, Fancy.Decoration.create () ])
    ();
  [%expect
    {|
    <pre>
      <span> . </span>



      <span> foo
    bar </span>



      <span> . </span>
    </pre>
    |}]
;;

let%expect_test "tokenization" =
  (* tokenize words and apply different decorations to them *)
  run_test
    ~text:"foo\nbar"
    ~process:(fun _ ->
      [ "foo", Fancy.Decoration.create ~color:(`Name "red") ()
      ; "bar", Fancy.Decoration.create ~color:(`Name "blue") ()
      ])
    ();
  [%expect
    {|
    <pre>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span style={ color: red; }> foo </span>


      <span style={ color: blue; }> bar </span>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </pre>
    |}]
;;

let%expect_test "bad tokenization" =
  (* In this test, we provide decorations for "baz" when it should have been "bar". The
     behavior of the component should be:
     - produce an error
     - ignore the bad decoration, and emit "bar" undecorated *)
  run_test
    ~text:"foo\nbar"
    ~process:(fun _ ->
      [ "foo", Fancy.Decoration.create ~color:(`Name "red") ()
      ; "baz", Fancy.Decoration.create ~color:(`Name "blue") ()
      ])
    ();
  [%expect
    {|
    ("Error applying decoration!" (at_char 4) (expected_prefix baz)
     (but_found bar) (decoration ((color ((Name blue))))))
    <pre>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span style={ color: red; }> foo </span>

    bar
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </pre>
    |}]
;;

let%expect_test "missing final token" =
  (* If the decorations don't cover all of the text, emit the remaining text without
     decorations. Maybe this should be considered an "error", but I think it's mostly fine
     tbh *)
  run_test
    ~text:"foo\nbar"
    ~process:(fun _ -> [ "foo", Fancy.Decoration.create ~color:(`Name "red") () ])
    ();
  [%expect
    {|
    <pre>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <span style={ color: red; }> foo </span>

    bar
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </pre>
    |}]
;;
