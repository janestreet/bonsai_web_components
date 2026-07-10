open! Core
open Bonsai_web
open Bonsai.Let_syntax
module Handle = Jsdom.Handle_experimental
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

let test_click_on_anchor ?on_click ~href () =
  Url_var.For_testing.reset url_var ~init:Page.Home;
  let%with handle = Handle.with_ ~get_vdom:Fn.id (anchor ?on_click ~href) in
  Handle.one_frame handle;
  Handle.click_on handle ~selector:"a";
  Handle.one_frame handle;
  Handle.print_dom handle
;;

let%expect_test "clicking on <a>" =
  test_click_on_anchor ~href:"/other-page" ();
  [%expect
    {|
    Opening in same tab: /other-page
    Intercepted navigation event
    ("Pushing to history" (new_location Other_page))
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> I'm on another page! </div>
      </body>
    </html>
    |}]
;;

let%expect_test "clicking on <a> whose href doesn't parse" =
  test_click_on_anchor ~href:"/not-a-page" ();
  [%expect
    {|
    Opening in same tab: /not-a-page
    Intercepted navigation event
    ("Pushing to history" (new_location Error))
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> I'm on the error page! </div>
      </body>
    </html>
    |}]
;;

let%expect_test "clicking on <a> with an external href" =
  test_click_on_anchor ~href:"https://www.janestreet.com/other-page" ();
  [%expect
    {|
    Opening external link in same tab: https://www.janestreet.com/other-page
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
           I'm on the home page!
          <a href="https://www.janestreet.com/other-page"> home page link </a>
        </div>
      </body>
    </html>
    |}]
;;

let%expect_test "clicking on <a> that prevents default" =
  test_click_on_anchor
    ~href:"/other-page"
    ~on_click:(Vdom.Attr.on_click (Effect.of_sync_fun (fun evt -> evt##preventDefault)))
    ();
  [%expect
    {|
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
           I'm on the home page!
          <a href="/other-page"> home page link </a>
        </div>
      </body>
    </html>
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
  let%with handle = Handle.with_ ~get_vdom:Fn.id (effect_open eff) in
  Handle.one_frame handle;
  Handle.click_on handle ~selector:"span";
  Handle.one_frame handle;
  Handle.print_dom handle
;;

let%expect_test "clicking on something with [Effect.open_url]" =
  test_click_on_effect_open ~href:"/other-page" ();
  [%expect
    {|
    Opening in same tab: /other-page
    Intercepted navigation event
    ("Pushing to history" (new_location Other_page))
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> I'm on another page! </div>
      </body>
    </html>
    |}]
;;

let%expect_test "[Effect.open_url] whose url doesn't parse" =
  test_click_on_effect_open ~href:"/not-a-page" ();
  [%expect
    {|
    Opening in same tab: /not-a-page
    Intercepted navigation event
    ("Pushing to history" (new_location Error))
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> I'm on the error page! </div>
      </body>
    </html>
    |}]
;;

let%expect_test "[Effect.open_url] with an external link" =
  test_click_on_effect_open ~href:"https://www.janestreet.com/other-page" ();
  [%expect
    {|
    Opening external link in same tab: https://www.janestreet.com/other-page
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
           I'm on the home page!
          <span> home page link </span>
        </div>
      </body>
    </html>
    |}]
;;

let%expect_test "[Effect.open_url] opening in a new tab or window" =
  test_click_on_effect_open ~in_:New_tab_or_window ~href:"/other-page" ();
  [%expect
    {|
    Opening in new tab/window: /other-page
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;">
           I'm on the home page!
          <span> home page link </span>
        </div>
      </body>
    </html>
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
    <html>
      <head>
        <meta charset="UTF-8"/>
      </head>
      <body>
        <div tabindex="0" style="outline: none;"> I'm on another page! </div>
      </body>
    </html>
    |}]
;;
