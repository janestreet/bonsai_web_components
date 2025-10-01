open! Core
open! Uri_parsing
module Url_var = Bonsai_web_ui_url_var
open! Uri_parsing_test.Test_util
open Bonsai_web
open Bonsai_web_test

let fire_navigation_event
  ?(can_intercept = true)
  ?download_request
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
          val url = Js_of_ocaml.Js.Unsafe.js_expr {|globalThis.location.href|}
        end
    end
  in
  let dispatch_navigation_event =
    Js_of_ocaml.Js.Unsafe.js_expr
      {js|(function(event) {
      globalThis.navigation.dispatchEvent(event);
    })|js}
  in
  let () =
    Js_of_ocaml.Js.Unsafe.call
      dispatch_navigation_event
      (Js_of_ocaml.Js.Unsafe.js_expr {|globalThis|})
      [| Js_of_ocaml.Js.Unsafe.inject event |]
  in
  Effect.return ()
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

let _url_var =
  Url_var.Typed.make
    ~trailing_slash_behavior:Keep_trailing_slashes
    ~navigation:`Ignore
    (module Url)
    parser_
    ~fallback:(fun _ -> failwith "Error!")
;;

let%expect_test "Ignore variants never intercept" =
  let%bind.With handle =
    Jsdom.Handle_experimental.with_
      ~get_vdom:(fun _ -> Vdom.Node.none)
      (fun _graph -> Bonsai.return ())
  in
  Url_var.For_testing.mock_required_browser_functionality ();
  let fire_event = fire_navigation_event (fun () -> print_s [%message "intercepted"]) in
  Jsdom.Handle_experimental.inject handle (fun () -> fire_event);
  [%expect {| |}]
;;
