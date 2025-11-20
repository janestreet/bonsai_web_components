open! Core
open! Bonsai_web
open! Bonsai_web_test
module Form = Bonsai_web_ui_form.With_manual_view

(* NOTE: This test case solely shows what happens when we set different form colors in
   [Form.Toggle.]. *)

let form ?colors (local_ graph) =
  Form.Elements.Toggle.bool ?colors ~default:false () graph
;;

let create_handle ?colors () =
  Handle.create
    (Bonsai_web_ui_form_manual_test.form_result_spec [%sexp_of: bool])
    (fun (local_ graph) -> form ?colors graph)
;;

let%expect_test "Default colors" =
  let handle = create_handle () in
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <label class="toggle_hash_replaced_in_test"
           custom-css-vars=((--bg-toggle_hash_replaced_in_test white)(--bg-on_hash_replaced_in_test #2196f3)(--bg-off_hash_replaced_in_test #ccc))>
      <input @key=bonsai_path_replaced_in_test
             type="checkbox"
             class="invisible_hash_replaced_in_test"
             #checked="false"
             @on_click
             style={
               margin-left: 0px;
             }/>
      <span class="slider_hash_replaced_in_test"> </span>
    </label>
    |}]
;;

let%expect_test "Custom colors" =
  let handle =
    create_handle
      ~colors:
        (Bonsai.return
           (Form.Elements.Toggle.Colors.create
              ~toggle:(`Name "tomato")
              ~background_on:(`Name "rebeccapurple")
              ~background_off:(`Name "green")
              ()))
      ()
  in
  Handle.show handle;
  [%expect
    {|
    (Ok false)

    ==============
    <label class="toggle_hash_replaced_in_test"
           custom-css-vars=((--bg-toggle_hash_replaced_in_test tomato)(--bg-on_hash_replaced_in_test rebeccapurple)(--bg-off_hash_replaced_in_test green))>
      <input @key=bonsai_path_replaced_in_test
             type="checkbox"
             class="invisible_hash_replaced_in_test"
             #checked="false"
             @on_click
             style={
               margin-left: 0px;
             }/>
      <span class="slider_hash_replaced_in_test"> </span>
    </label>
    |}]
;;
