open! Core
open Bonsai_web
open! Uri_parsing
module Url_var = Bonsai_web_url_var
open! Uri_parsing_test.Test_util
module Handle = Jsdom.Handle_experimental

let fire_navigation_event
  ?(can_intercept = true)
  ?download_request
  ?(navigation_type = "push")
  ~destination
  (intercept : unit -> unit)
  =
  let event =
    object%js
      val canIntercept = Js_of_ocaml.Js.bool can_intercept
      val navigationType = Js_of_ocaml.Js.string navigation_type

      val downloadRequest =
        match download_request with
        | None -> Js_of_ocaml.Js.Opt.empty
        | Some download_request -> Js_of_ocaml.Js.Opt.return download_request

      val type_ = Js_of_ocaml.Js.string "navigate"
      val intercept = Js_of_ocaml.Js.wrap_callback (fun _ -> intercept ())

      val destination =
        object%js
          val url = Js_of_ocaml.Js.string destination
        end
    end
  in
  let dispatch_navigation_event =
    Js_of_ocaml.Js.Unsafe.js_expr
      {js|(function(event) {
      globalThis.navigation.dispatchEvent(event);
    })|js}
  in
  let effect =
    Effect.of_sync_fun (fun () ->
      let () =
        Js_of_ocaml.Js.Unsafe.fun_call
          dispatch_navigation_event
          [| Js_of_ocaml.Js.Unsafe.inject event |]
      in
      ())
  in
  effect ()
;;

module Url = struct
  type t =
    | Normal of int
    | Fallback
    | Catch_all
  [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Normal -> Parser.from_path Value_parser.int
    | Fallback -> Parser.unit
    | Catch_all -> Parser.unit
  ;;
end

let parser_ =
  let parser = Parser.Variant.make (module Url) in
  Versioned_parser.first_parser parser
;;

let%expect_test "Fails to add intercept handler if \
                 [For_testing.mock_required_browser_functionality] hasn't been called"
  =
  let%bind.With _handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  let _url_var =
    Url_var.Typed.make
      ~trailing_slash_behavior:Keep_trailing_slashes
      ~navigation:(`Intercept `Reset_scroll)
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  [%expect
    {|
     "Unable to attach intercept handler in tests. If you want to test this \
    \n functionality, please call [For_testing.mock_browser_functionality_for_tests] \
    \n  \
    \n            before creating the URL var."
    |}]
;;

let%expect_test "`Intercept actually calls the intercept event" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
  let _url_var =
    Url_var.Typed.make
      ~trailing_slash_behavior:Keep_trailing_slashes
      ~navigation:(`Intercept `Reset_scroll)
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  let fire_event = fire_navigation_event Fn.id ~destination:"/normal/3" in
  Handle.inject handle (fun () -> fire_event);
  [%expect {| Intercepted navigation event |}]
;;

let%expect_test "`Intercept is bypassed with reload effect" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
  let _url_var =
    Url_var.Typed.make
      ~trailing_slash_behavior:Keep_trailing_slashes
      ~navigation:(`Intercept `Reset_scroll)
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  Handle.inject handle (fun () -> Bonsai_web_url_var.reload_without_intercepting);
  [%expect {| |}]
;;

let%expect_test "`Intercept is bypassed by regular reload events" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
  let _url_var =
    Url_var.Typed.make
      ~trailing_slash_behavior:Keep_trailing_slashes
      ~navigation:(`Intercept `Reset_scroll)
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  let fire_event =
    fire_navigation_event Fn.id ~navigation_type:"reload" ~destination:"/"
  in
  Handle.inject handle (fun () -> fire_event);
  [%expect {| |}]
;;

let%expect_test "base-path navigation updates state only for paths belonging to the app" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.set_base_path_for_testing "graphs";
  Exn.protect ~finally:Url_var.For_testing.clear_base_path_for_testing ~f:(fun () ->
    Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
    let url_var =
      Url_var.Typed.make
        ~trailing_slash_behavior:Keep_trailing_slashes
        (module Url)
        parser_
        ~fallback:(fun _ _ -> Url.Catch_all)
    in
    let navigate destination =
      Handle.inject handle (fun () -> Effect.open_url destination);
      print_s [%sexp (Url_var.get url_var : Url.t)]
    in
    navigate "/graphs/normal/3";
    [%expect
      {|
      Opening in same tab: /graphs/normal/3
      Intercepted navigation event
      ("Pushing to history" (new_location (Normal 3)))
      (Normal 3)
      |}];
    navigate "/graphs/normal/not-an-int";
    [%expect
      {|
      Opening in same tab: /graphs/normal/not-an-int
      Intercepted navigation event
      ("Pushing to history" (new_location Catch_all))
      Catch_all
      |}];
    navigate "/other/normal/4";
    [%expect
      {|
      Opening in same tab: /other/normal/4
      Catch_all
      |}];
    navigate "/graphs2/normal/4";
    [%expect
      {|
      Opening in same tab: /graphs2/normal/4
      Catch_all
      |}];
    let current_uri =
      Js_of_ocaml.Dom_html.window##.location##.href
      |> Js_of_ocaml.Js.to_string
      |> Uri.of_string
    in
    let destination = Uri.with_path current_uri "//graphs/normal/4" |> Uri.to_string in
    let fire_event =
      fire_navigation_event ~destination (fun () -> print_endline "intercept called")
    in
    Handle.inject handle (fun () -> fire_event);
    [%expect {| |}])
;;

let with_current_url url ~f =
  let open Js_of_ocaml in
  let window = Dom_html.window in
  let history = window##.history in
  let original_url = window##.location##.href in
  let original_state = history##.state in
  Exn.protect
    ~finally:(fun () ->
      history##replaceState original_state (Js.string "") (Js.some original_url))
    ~f:(fun () ->
      history##replaceState Js.null (Js.string "") (Js.some (Js.string url));
      f ())
;;

let with_base_href href ~f =
  let open Js_of_ocaml in
  let document = Dom_html.window##.document in
  let base = Dom_html.createBase document in
  base##.href := Js.string href;
  Dom.appendChild document##.head base;
  Exn.protect ~finally:(fun () -> Dom.removeChild document##.head base) ~f
;;

let%expect_test "sexp URL updates preserve the browser path without doubling the base \
                 path"
  =
  let module Sexp_url = Url_var.Literally_just_a_gigantic_sexp (Int) in
  let%bind.With _handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  let open Js_of_ocaml in
  let history = Dom_html.window##.history in
  with_current_url "/graphs/view?query=1#details" ~f:(fun () ->
    with_base_href "/graphs/" ~f:(fun () ->
      let components = Sexp_url.unparse 2 in
      print_s [%sexp (components : Url_var.Components.t)];
      [%expect {| ((path /view) (query ((query (2)))) (fragment (details))) |}];
      let uri = Url_var.Components.to_path_and_query components in
      let path = Uri.path uri in
      let query = Uri.query uri in
      let fragment = Uri.fragment uri in
      print_s
        [%message
          (path : string) (query : (string * string list) list) (fragment : string option)];
      [%expect {| ((path /graphs/view) (query ((query (2)))) (fragment (details))) |}];
      let round_trip path =
        history##replaceState Js.null (Js.string "") (Js.some (Js.string path));
        Sexp_url.unparse 2
        |> Url_var.Components.to_path_and_query
        |> Uri.path
        |> print_endline
      in
      round_trip "/graphs/";
      [%expect {| /graphs/ |}];
      round_trip "/graphs//view";
      [%expect {| /graphs//view |}];
      round_trip "/graphs";
      [%expect {| /graphs |}]))
;;

let test_relative_anchor ~navigation =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  with_current_url "/graphs/nested/current" ~f:(fun () ->
    let module Js = Js_of_ocaml.Js in
    let module Dom_html = Js_of_ocaml.Dom_html in
    let module Dom = Js_of_ocaml.Dom in
    let document = Dom_html.window##.document in
    let current_uri =
      Dom_html.window##.location##.href |> Js.to_string |> Uri.of_string
    in
    print_endline (Url_var.Components.of_uri current_uri).path;
    with_base_href "/graphs/" ~f:(fun () ->
      Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
      let url_var =
        Url_var.Typed.make (module Url) parser_ ~navigation ~fallback:(fun _ _ ->
          Url.Catch_all)
      in
      let anchor = Dom_html.createA document in
      let href =
        Url_var.Typed.to_url_string (Parser.Variant.make (module Url)) (Normal 3)
      in
      print_endline href;
      anchor##.href := Js.string href;
      anchor##.id := Js.string "relative-link";
      let destination = ref None in
      (* [Handle.click_on] mocks navigation using the raw href. Capture the DOM-resolved
         destination instead; JSDOM does not implement actual page navigation. *)
      anchor##.onclick
      := Dom_html.handler (fun event ->
           event##preventDefault;
           destination := Some (Js.to_string anchor##.href);
           Js.bool false);
      Dom.appendChild document##.body anchor;
      Exn.protect
        ~finally:(fun () -> Dom.removeChild document##.body anchor)
        ~f:(fun () ->
          Handle.click_on handle ~selector:"#relative-link";
          let destination = Option.value_exn !destination in
          Handle.inject handle (fun () -> Effect.open_url destination);
          print_s [%sexp (Url_var.get url_var : Url.t)])))
;;

let%expect_test "base resolves relative anchors without interception" =
  test_relative_anchor ~navigation:`Ignore;
  [%expect
    {|
    graphs/nested/current
    normal/3
    Opening in same tab: https://localhost/graphs/normal/3
    Catch_all
    |}]
;;

let%expect_test "base resolves relative anchors with interception" =
  test_relative_anchor ~navigation:(`Intercept `Reset_scroll);
  [%expect
    {|
    graphs/nested/current
    normal/3
    Opening in same tab: https://localhost/graphs/normal/3
    Intercepted navigation event
    ("Pushing to history" (new_location (Normal 3)))
    (Normal 3)
    |}]
;;

let%expect_test "unsupported bases leave URL-var paths unchanged" =
  let%bind.With _handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  let test href =
    with_base_href href ~f:(fun () ->
      let result =
        Or_error.try_with (fun () ->
          let uri = Uri.of_string "/graphs/view" in
          (Url_var.Components.of_uri uri).path)
      in
      print_s [%sexp (result : string Or_error.t)])
  in
  test "/graphs";
  [%expect {| (Ok graphs/view) |}];
  test "https://example.com/graphs/";
  [%expect {| (Ok graphs/view) |}]
;;

let%expect_test "DOM lookup errors leave URL-var paths unchanged" =
  let%bind.With _handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  with_base_href "/graphs/" ~f:(fun () ->
    let open Js_of_ocaml in
    let with_broken_query_selector =
      Js.Unsafe.js_expr
        {js|(function(document, f) {
          const original = document.querySelector;
          try {
            document.querySelector = () => { throw new Error("DOM lookup failed"); };
            f();
          } finally {
            document.querySelector = original;
          }
        })|js}
    in
    let () =
      Js.Unsafe.fun_call
        with_broken_query_selector
        [| Js.Unsafe.inject Dom_html.window##.document
         ; Js.Unsafe.inject
             (Js.wrap_callback (fun () ->
                let components =
                  Url_var.Components.of_uri (Uri.of_string "/graphs/view")
                in
                print_endline components.path))
        |]
    in
    [%expect {| graphs/view |}])
;;
