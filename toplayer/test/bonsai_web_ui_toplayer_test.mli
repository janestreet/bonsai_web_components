open! Core
open Bonsai_web
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

(* The following are untestable without a DOM simulator:
   - Actual positioning of the popover
   - `showPopover`/`hidePopover on the popover DOM element, because we can't run hook
     logic
   - close on click / escape outside, because we don't can't run global listener logic, or
     test event propagation.
*)
module Test_selectors : sig
  val app_root : Bonsai.Test_selector.t
  val anchored_popovers : Bonsai.Test_selector.t
  val virtual_popovers : Bonsai.Test_selector.t
  val modals : Bonsai.Test_selector.t
end

(** Tests for components that use toplayer elements should wrap their vdom in this
    function before passing it to [Handle.create].

    The output Vdom will include the content of all popovers and modals. *)
val wrap_app_vdom : ?verbose:bool -> Vdom.Node.t Bonsai.t -> Vdom.Node.t Bonsai.t
