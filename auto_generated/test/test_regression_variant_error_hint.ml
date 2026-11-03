open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import
open Test_util

let%expect_test "variant with no clause selected shows an error indicator" =
  let module T = struct
    type t =
      | A
      | B of int
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <table>
      <tbody>
        <tr>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <select id="bonsai_path_replaced_in_test"
                    class="widget-dropdown"
                    @on_change
                    style={
                      width: 100.00%;
                    }>
              <option value="0" #selected="true">  </option>
              <option value="1" #selected="false"> a </option>
              <option value="2" #selected="false"> b </option>
            </select>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
              <div class="container_hash_replaced_in_test">
                <label class="label_hash_replaced_in_test" style={ color: #f54646; }>
                  <input type="checkbox" tabindex="-1" class="checkbox_hash_replaced_in_test"/>
                  <span class="span_hash_replaced_in_test"> ⚠ </span>
                  <div class="right_hash_replaced_in_test text_hash_replaced_in_test"
                       style={
                         border: 1px solid red;
                         color: black;
                         background-color: pink;
                       }>
                    <div>
                      <div>
                        <pre> "a value is required" </pre>
                      </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}];
  Handle.do_actions handle [ T.A ];
  Handle.show_diff handle;
  [%expect
    {|
    -|(Error "a value is required")
    +|(Ok A)

      ==============
      <table>
        <tbody>
          <tr>
            <td style={
                  padding-left: 0em;
                  padding-right: 1em;
                  text-align: left;
                  font-weight: bold;
                  user-select: none;
                }>  </td>
            <td>
              <select id="bonsai_path_replaced_in_test"
                      class="widget-dropdown"
                      @on_change
                      style={
                        width: 100.00%;
                      }>
    -|          <option value="0" #selected="true">  </option>
    +|          <option value="0" #selected="false">  </option>
    -|          <option value="1" #selected="false"> a </option>
    +|          <option value="1" #selected="true"> a </option>
                <option value="2" #selected="false"> b </option>
              </select>
            </td>
    -|      <td>
    -|        <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
    -|          <div class="container_hash_replaced_in_test">
    -|            <label class="label_hash_replaced_in_test" style={ color: #f54646; }>
    -|              <input type="checkbox" tabindex="-1" class="checkbox_hash_replaced_in_test"/>
    -|              <span class="span_hash_replaced_in_test"> ⚠ </span>
    -|              <div class="right_hash_replaced_in_test text_hash_replaced_in_test"
    -|                   style={
    -|                     border: 1px solid red;
    -|                     color: black;
    -|                     background-color: pink;
    -|                   }>
    -|                <div>
    -|                  <div>
    -|                    <pre> "a value is required" </pre>
    -|                  </div>
    -|                </div>
    -|              </div>
    -|            </label>
    -|          </div>
    -|        </div>
    -|      </td>
          </tr>
        </tbody>
      </table>
    |}]
;;

let%expect_test "union of two variants with no clause selected shows an error indicator" =
  let module T = struct
    type global =
      [ `Inherit
      | `Initial
      ]
    [@@deriving sexp, sexp_grammar]

    type t =
      [ `Var of string
      | global
      ]
    [@@deriving sexp, sexp_grammar]
  end
  in
  let handle = sexp_form_handle ~get_vdom:get_vdom_detailed (module T) in
  Handle.show handle;
  [%expect
    {|
    (Error "a value is required")

    ==============
    <table>
      <tbody>
        <tr>
          <td style={
                padding-left: 0em;
                padding-right: 1em;
                text-align: left;
                font-weight: bold;
                user-select: none;
              }>  </td>
          <td>
            <select id="bonsai_path_replaced_in_test"
                    class="widget-dropdown"
                    @on_change
                    style={
                      width: 100.00%;
                    }>
              <option value="0" #selected="true">  </option>
              <option value="1" #selected="false"> inherit </option>
              <option value="2" #selected="false"> initial </option>
              <option value="3" #selected="false"> var </option>
            </select>
          </td>
          <td>
            <div style={ display: flex; flex-direction: row; flex-wrap: nowrap; }>
              <div class="container_hash_replaced_in_test">
                <label class="label_hash_replaced_in_test" style={ color: #f54646; }>
                  <input type="checkbox" tabindex="-1" class="checkbox_hash_replaced_in_test"/>
                  <span class="span_hash_replaced_in_test"> ⚠ </span>
                  <div class="right_hash_replaced_in_test text_hash_replaced_in_test"
                       style={
                         border: 1px solid red;
                         color: black;
                         background-color: pink;
                       }>
                    <div>
                      <div>
                        <pre> "a value is required" </pre>
                      </div>
                    </div>
                  </div>
                </label>
              </div>
            </div>
          </td>
        </tr>
      </tbody>
    </table>
    |}]
;;
