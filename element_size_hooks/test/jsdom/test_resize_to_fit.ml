open! Core
open! Bonsai_web
open Js_of_ocaml
open Bonsai_web_element_size_hooks
module Handle = Jsdom.Handle_experimental

(* [Virtual_dom] calls a hook's [update] function on every patch of the element, even when
   the hook's input is unchanged. These tests verify that patching an element with an
   unchanged [Resize_to_fit] behavior does not rewrite its styles, while an actual
   behavior change still does.

   The hook manages the [width] and [transform] style properties, but jsdom's CSSOM
   rejects the [max-content] / [fit-content] width values the hook uses, so these tests
   observe [transform]: the hook's [update] sets it whenever it runs (in these tests to
   [scale(1)], since jsdom reports all dimensions as zero), while [init] does not set it
   at all.

   Note also that we never run the requestAnimationFrame tasks, because the hook's
   [on_mount] constructs a [ResizeObserver], which jsdom does not implement. This is why a
   warning about a non-empty requestAnimationFrame queue is printed on cleanup. *)

let print_target_transform () =
  let target =
    Js.Opt.get
      (Dom_html.document##querySelector (Js.string "#target"))
      (fun () -> failwith "no element with id [target]")
  in
  let transform = Js.to_string (Js.Unsafe.get target##.style (Js.string "transform")) in
  print_s [%message (transform : string)]
;;

let%expect_test "patching with an unchanged behavior is a no-op; changing the behavior \
                 is not"
  =
  let computation graph =
    let open Bonsai.Let_syntax in
    let behavior, set_behavior =
      Bonsai.state Resize_to_fit.Behavior.Shrink_to_avoid_overflow graph
    in
    let redraw_count, set_redraw_count = Bonsai.state 0 graph in
    let%arr behavior and set_behavior and redraw_count and set_redraw_count in
    let vdom =
      (* The whole vdom depends on [redraw_count], so bumping it forces a fresh vdom node
         for the target element, which makes [Virtual_dom] diff it and call the hook's
         [update]. *)
      Vdom.Node.div
        [ Vdom.Node.span
            ~attrs:[ Vdom.Attr.id "target"; Resize_to_fit.attr ~behavior () ]
            [ Vdom.Node.text "content" ]
        ; Vdom.Node.text [%string "redraws: %{redraw_count#Int}"]
        ]
    in
    vdom, set_behavior, set_redraw_count (redraw_count + 1)
  in
  Handle.with_
    ~get_vdom:(fun (vdom, _, _) -> vdom)
    computation
    (fun handle ->
      Handle.one_frame handle;
      (* [init] does not set the transform. *)
      print_target_transform ();
      [%expect {| (transform "") |}];
      (* Patch with the behavior unchanged: the transform must not be set. *)
      Handle.inject handle (fun (_, _, redraw) -> redraw);
      Handle.one_frame handle;
      print_target_transform ();
      [%expect {| (transform "") |}];
      (* Patch with a changed behavior: the hook must adjust the transform. *)
      Handle.inject handle (fun (_, set_behavior, _) ->
        set_behavior Resize_to_fit.Behavior.Grow_to_fill);
      Handle.one_frame handle;
      print_target_transform ();
      [%expect {| (transform "scale(1)") |}]);
  (* This warning is expected: we deliberately never ran the requestAnimationFrame tasks
     (see the comment at the top of this file). *)
  [%expect {| WARNING: requestAnimationFrame queue not empty upon cleanup |}]
;;
