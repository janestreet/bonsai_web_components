open! Core
open Bonsai_web
open! Uri_parsing
module Url_var = Bonsai_web_ui_url_var
open! Uri_parsing_test.Test_util
module Handle = Jsdom.Handle_experimental

let fire_navigation_event
  ?(can_intercept = true)
  ?download_request
  ~destination
  (intercept : unit -> unit)
  =
  let event =
    object%js
      val canIntercept = Js_of_ocaml.Js.bool can_intercept

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
      ~navigation:`Intercept
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
      ~navigation:`Intercept
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  let fire_event = fire_navigation_event Fn.id ~destination:"/normal/3" in
  Handle.inject handle (fun () -> fire_event);
  [%expect {| Intercepted navigation event |}]
;;

let%expect_test "`Intercept can be bypassed with special-cased reload method" =
  let%bind.With handle =
    Handle.with_ ~get_vdom:(fun _ -> Vdom.Node.none) (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.mock_required_browser_functionality_for_navigation_intercept ();
  let _url_var =
    Url_var.Typed.make
      ~trailing_slash_behavior:Keep_trailing_slashes
      ~navigation:`Intercept
      (module Url)
      parser_
      ~fallback:(fun _ _ -> Url.Catch_all)
  in
  Handle.inject handle (fun () -> Bonsai_web_ui_url_var.reload_without_intercepting);
  [%expect {| |}]
;;
