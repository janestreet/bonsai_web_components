open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Handle = Bonsai_web_test.Handle
module Url_var = Bonsai_web_url_var

module Page = struct
  type t =
    | Home [@index]
    | Other_page
    | Error
  [@@deriving sexp, equal, typed_variants, uri_parsing]

  let parser = Uri_parsing.Versioned_parser.first_parser parser
end

(* Note that [Url_var.For_testing.reset] must be called on this URL var at the beginning
   of every test in order for [navigation:`Intercept] to work properly. *)
let url_var =
  Url_var.Typed.make
    ~navigation:`Intercept
    (module Page)
    ~fallback:(fun _ _ -> Page.Error)
    Page.parser
;;

let anchor ?on_click ~href (local_ _graph) =
  match%arr Url_var.value url_var with
  | Page.Home ->
    {%html|
      <div>
        #{" I'm on the home page! "}<a href=%{href} ?{on_click}
          >#{"home page link"}</a
        >
      </div>
    |}
  | Other_page -> {%html|<div>I'm on another page!</div>|}
  | Error -> {%html|<div>I'm on the error page!</div>|}
;;

let test_click_on_anchor ?ctrl_key_down ?on_click ~href () =
  Url_var.For_testing.reset url_var ~init:Page.Home;
  let handle =
    Handle.create (Bonsai_web_test.Result_spec.vdom Fn.id) (anchor ?on_click ~href)
  in
  Handle.recompute_view handle;
  Handle.click_on ?ctrl_key_down handle ~get_vdom:Fn.id ~selector:"a";
  Handle.recompute_view handle;
  Handle.show handle
;;

let%expect_test "clicking on <a>" =
  test_click_on_anchor ~href:"/other-page" ();
  [%expect
    {|
    Opening in same tab: /other-page
    Intercepted navigation event
    ("Pushing to history" (new_location Other_page))
    <div> I'm on another page! </div>
    |}]
;;

let%expect_test "clicking on <a> whose href doesn't parse" =
  test_click_on_anchor ~href:"/not-a-page" ();
  [%expect
    {|
    Opening in same tab: /not-a-page
    Intercepted navigation event
    ("Pushing to history" (new_location Error))
    <div> I'm on the error page! </div>
    |}]
;;

let%expect_test "clicking on <a> with an external href" =
  test_click_on_anchor ~href:"https://www.janestreet.com/other-page" ();
  [%expect
    {|
    Opening external link in same tab: https://www.janestreet.com/other-page
    <div>
       I'm on the home page!
      <a href="https://www.janestreet.com/other-page"> home page link </a>
    </div>
    |}]
;;

let%expect_test "ctrl clicking on <a>" =
  test_click_on_anchor ~href:"/other-page" ~ctrl_key_down:true ();
  [%expect
    {|
    Opening in new tab/window: /other-page
    <div>
       I'm on the home page!
      <a href="/other-page"> home page link </a>
    </div>
    |}]
;;

let%expect_test "clicking on <a> that prevents default" =
  test_click_on_anchor
    ~href:"/other-page"
    ~on_click:(Vdom.Attr.on_click (Effect.of_sync_fun (fun evt -> evt##preventDefault)))
    ();
  [%expect
    {|
    <div>
       I'm on the home page!
      <a href="/other-page" @on_click> home page link </a>
    </div>
    |}]
;;

let effect_open effect_open (local_ _graph) =
  match%arr Url_var.value url_var with
  | Page.Home ->
    {%html|
      <div>
        #{" I'm on the home page! "}<span on_click=%{effect_open}
          >#{"home page link"}</span
        >
      </div>
    |}
  | Other_page -> {%html|<div>I'm on another page!</div>|}
  | Error -> {%html|<div>I'm on the error page!</div>|}
;;

let test_click_on_effect_open ?in_ ?(on_click = fun _ -> Effect.Ignore) ~href () =
  Url_var.For_testing.reset url_var ~init:Page.Home;
  let eff evt = Effect.Many [ on_click evt; Effect.open_url ?in_ href ] in
  let handle = Handle.create (Bonsai_web_test.Result_spec.vdom Fn.id) (effect_open eff) in
  Handle.recompute_view handle;
  Handle.click_on handle ~get_vdom:Fn.id ~selector:"span";
  Handle.recompute_view handle;
  Handle.show handle
;;

let%expect_test "clicking on something with [Effect.open_url]" =
  test_click_on_effect_open ~href:"/other-page" ();
  [%expect
    {|
    Opening in same tab: /other-page
    Intercepted navigation event
    ("Pushing to history" (new_location Other_page))
    <div> I'm on another page! </div>
    |}]
;;

let%expect_test "[Effect.open_url] whose url doesn't parse" =
  test_click_on_effect_open ~href:"/not-a-page" ();
  [%expect
    {|
    Opening in same tab: /not-a-page
    Intercepted navigation event
    ("Pushing to history" (new_location Error))
    <div> I'm on the error page! </div>
    |}]
;;

let%expect_test "[Effect.open_url] with an external link" =
  test_click_on_effect_open ~href:"https://www.janestreet.com/other-page" ();
  [%expect
    {|
    Opening external link in same tab: https://www.janestreet.com/other-page
    <div>
       I'm on the home page!
      <span @on_click> home page link </span>
    </div>
    |}]
;;

let%expect_test "[Effect.open_url] opening in a new tab or window" =
  test_click_on_effect_open ~in_:New_tab_or_window ~href:"/other-page" ();
  [%expect
    {|
    Opening in new tab/window: /other-page
    <div>
       I'm on the home page!
      <span @on_click> home page link </span>
    </div>
    |}]
;;

let%expect_test "[Effect.open_url] disregards [preventDefault] (correct behavior)" =
  test_click_on_effect_open
    ~href:"/other-page"
    ~on_click:(Effect.of_sync_fun (fun evt -> evt##preventDefault))
    ();
  [%expect
    {|
    Opening in same tab: /other-page
    Intercepted navigation event
    ("Pushing to history" (new_location Other_page))
    <div> I'm on another page! </div>
    |}]
;;
