open! Core
open Jsdom
module Handle = Handle_experimental

let default_selector = "[popover]"

let assert_open_but_not_shown
  ?(here = Stdlib.Lexing.dummy_pos)
  ?(selector = default_selector)
  handle
  =
  Handle.query_selector
    handle
    ~selector:[%string "%{selector}:not([mock-popover-state=\"open\"])"]
  |> Expect_test_helpers_core.require_some ~here
;;

let assert_open_and_shown
  ?(here = Stdlib.Lexing.dummy_pos)
  ?(selector = default_selector)
  handle
  =
  Handle.query_selector
    handle
    ~selector:[%string "%{selector}[mock-popover-state=\"open\"]"]
  |> Expect_test_helpers_core.require_some ~here
;;

let assert_not_in_dom
  ?(here = Stdlib.Lexing.dummy_pos)
  ?(selector = default_selector)
  handle
  =
  Handle.query_selector handle ~selector:[%string "%{selector}"]
  |> Expect_test_helpers_core.require_none (fun _ -> Sexp.Atom "<opaque dom node>") ~here
;;
