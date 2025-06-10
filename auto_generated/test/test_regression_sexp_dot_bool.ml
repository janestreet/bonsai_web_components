open! Core
open! Bonsai_web
open! Bonsai_web_test
open! Bonsai.Let_syntax
open! Import
open Test_util

type the_record = { a : bool }

module Normal_record = struct
  type t = the_record = { a : bool } [@@deriving sexp, sexp_grammar]
end

module Record_with_sexp_dot_bool = struct
  type t = the_record = { a : bool [@sexp.bool] } [@@deriving sexp, sexp_grammar]
end

let bisimulate_both_records ~f =
  f
    (module Normal_record : Test_util.S with type t = the_record)
    ~expect_diff:(fun ~normal_record ~sexp_dot_bool_record:_ -> normal_record ());
  f
    (module Record_with_sexp_dot_bool)
    ~expect_diff:(fun ~normal_record:_ ~sexp_dot_bool_record -> sexp_dot_bool_record ())
;;

let%expect_test "normal bool field" =
  bisimulate_both_records ~f:(fun t ~expect_diff ->
    let handle = sexp_form_handle t in
    Handle.show handle;
    expect_diff
      ~normal_record:(fun () ->
        [%expect
          {|
          (Ok ((a false)))

          ==============
          <input @key=bonsai_path_replaced_in_test
                 type="checkbox"
                 id="bonsai_path_replaced_in_test"
                 #checked="false"
                 @on_click
                 style={
                   margin-left: 0px;
                 }/>
          |}])
      ~sexp_dot_bool_record:(fun () ->
        [%expect
          {|
          (Ok ())

          ==============
          <div class="checkbox-container">
            <label>
              <input type="checkbox" #checked="false" @on_click/>

            </label>
          </div>
          |}]);
    Handle.do_actions handle [ { a = true } ];
    Handle.show_diff handle;
    expect_diff
      ~normal_record:(fun () ->
        [%expect
          {|
          -|(Ok ((a false)))
          +|(Ok ((a true)))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="checkbox"
                   id="bonsai_path_replaced_in_test"
          -|       #checked="false"
          +|       #checked="true"
                   @on_click
                   style={
                     margin-left: 0px;
                   }/>
          |}])
      ~sexp_dot_bool_record:(fun () ->
        [%expect
          {|
          -|(Ok ())
          +|(Ok ((a)))

            ==============
            <div class="checkbox-container">
              <label>
          -|    <input type="checkbox" #checked="false" @on_click/>
          +|    <input type="checkbox" #checked="true" @on_click/>

              </label>
            </div>
          |}]);
    Handle.do_actions handle [ { a = false } ];
    Handle.show_diff handle;
    expect_diff
      ~normal_record:(fun () ->
        [%expect
          {|
          -|(Ok ((a true)))
          +|(Ok ((a false)))

            ==============
            <input @key=bonsai_path_replaced_in_test
                   type="checkbox"
                   id="bonsai_path_replaced_in_test"
          -|       #checked="true"
          +|       #checked="false"
                   @on_click
                   style={
                     margin-left: 0px;
                   }/>
          |}])
      ~sexp_dot_bool_record:(fun () ->
        [%expect
          {|
          -|(Ok ((a)))
          +|(Ok ())

            ==============
            <div class="checkbox-container">
              <label>
          -|    <input type="checkbox" #checked="true" @on_click/>
          +|    <input type="checkbox" #checked="false" @on_click/>

              </label>
            </div>
          |}]))
;;

let%expect_test "Minimal repro" =
  (* This repro avoids using sexp serialization. Showing that this incorrectly sets the
     value. *)
  let module Result_spec = struct
    type t = Record_with_sexp_dot_bool.t Form.t
    type incoming = Set of bool

    let view t =
      match Form.value t with
      | Error error -> Error.to_string_hum error
      | Ok { a } -> Bool.to_string a
    ;;

    let incoming t (Set value) = Form.set t { a = value }
  end
  in
  let handle =
    Handle.create (module Result_spec) (fun (local_ graph) ->
      let form = Auto_generated.form (module Record_with_sexp_dot_bool) graph in
      let () =
        Bonsai.Edge.lifecycle
          ~on_activate:
            (let%arr form in
             Form.set form { a = true })
          graph
      in
      form)
  in
  Handle.show handle;
  [%expect {| false |}];
  Handle.do_actions handle [ Set true ];
  Handle.show handle;
  [%expect {| true |}];
  Handle.do_actions handle [ Set false ];
  Handle.show handle;
  [%expect {| false |}];
  Handle.do_actions handle [ Set true ];
  Handle.show handle;
  [%expect {| true |}]
;;

let%expect_test "Minimal repro with two record fields. It also tests that other \
                 non-required fields ([@sexp.option]) interact well with this fix."
  =
  (* This repro avoids using sexp serialization. Showing that this incorrectly sets the
     value. *)
  let module Result_spec = struct
    type record =
      { a : bool
      ; b : bool [@sexp.bool]
      ; c : int option [@sexp.option]
      }
    [@@deriving sexp, sexp_grammar]

    type t = record Form.t

    type incoming =
      | Set of
          { a : bool
          ; b : bool
          ; c : int option
          }

    let view t =
      match Form.value t with
      | Error error -> Error.to_string_hum error
      | Ok { a; b; c } ->
        let c =
          match c with
          | None -> "None"
          | Some c -> Int.to_string c
        in
        [%string "a: %{a#Bool}, b: %{b#Bool}, c: %{c}"]
    ;;

    let incoming t (Set { a; b; c }) = Form.set t { a; b; c }
  end
  in
  let handle =
    Handle.create (module Result_spec) (fun (local_ graph) ->
      let form =
        Auto_generated.form
          (module struct
            type t = Result_spec.record [@@deriving sexp, sexp_grammar]
          end)
          graph
      in
      let () =
        Bonsai.Edge.lifecycle
          ~on_activate:
            (let%arr form in
             Form.set form { a = true; b = true; c = None })
          graph
      in
      form)
  in
  Handle.show handle;
  [%expect {| a: false, b: false, c: None |}];
  Handle.do_actions handle [ Set { a = true; b = true; c = None } ];
  Handle.show handle;
  [%expect {| a: true, b: true, c: None |}];
  Handle.do_actions handle [ Set { a = false; b = false; c = None } ];
  Handle.show handle;
  [%expect {| a: false, b: false, c: None |}];
  Handle.do_actions handle [ Set { a = true; b = true; c = None } ];
  Handle.show handle;
  [%expect {| a: true, b: true, c: None |}];
  Handle.do_actions handle [ Set { a = true; b = false; c = Some 2 } ];
  Handle.show handle;
  [%expect {| a: true, b: false, c: 2 |}]
;;
