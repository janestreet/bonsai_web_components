open! Core
open Bonsai_web
open! Bonsai_web_test
open Bonsai.Let_syntax

module View = struct
  type t =
    { requested : Bonsai_web_css_color_scheme.Requested_mode.t
    ; resolved : Bonsai_web_css_color_scheme.Resolved_mode.t
    }
  [@@deriving sexp_of]
end

module Spec = struct
  type t =
    { view : View.t
    ; set : Bonsai_web_css_color_scheme.Requested_mode.t -> unit Effect.t
    ; toggle : unit Effect.t
    }

  type incoming =
    | Set of Bonsai_web_css_color_scheme.Requested_mode.t
    | Toggle

  let view { view; _ } = Sexp.to_string_hum [%sexp (view : View.t)]

  let incoming { set; toggle; _ } = function
    | Set mode -> set mode
    | Toggle -> toggle
  ;;
end

let getters_and_setters (local_ graph) =
  let%arr requested = Bonsai_web_css_color_scheme.requested_mode graph
  and resolved = Bonsai_web_css_color_scheme.resolved_mode graph
  and set = Bonsai_web_css_color_scheme.set_requested_mode graph
  and toggle = Bonsai_web_css_color_scheme.toggle graph in
  { Spec.view = { View.requested; resolved }; set; toggle }
;;

(* The library-owned fallback state is the one [install] provides. In tests there is no
   browser, so it resolves [System] to [Light]. The bare getters/setters raise without an
   installed state; see "the getters and setters raise without an installed state" at the
   end of this file. *)
let installed (local_ graph) =
  Bonsai_web_css_color_scheme.install getters_and_setters graph
;;

let%expect_test "install: defaults" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  let handle = Handle.create (module Spec) installed in
  Handle.show handle;
  [%expect {| ((requested System) (resolved Light)) |}]
;;

let%expect_test "install: setting the requested mode" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  let handle = Handle.create (module Spec) installed in
  Handle.do_actions handle [ Set Dark ];
  Handle.show handle;
  [%expect {| ((requested Dark) (resolved Dark)) |}];
  Handle.do_actions handle [ Set Light ];
  Handle.show handle;
  [%expect {| ((requested Light) (resolved Light)) |}];
  Handle.do_actions handle [ Set System ];
  Handle.show handle;
  [%expect {| ((requested System) (resolved Light)) |}]
;;

let%expect_test "install: toggle flips the resolved mode" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  let handle = Handle.create (module Spec) installed in
  (* [System] resolves to [Light] here, so the first toggle requests [Dark]. *)
  Handle.do_actions handle [ Toggle ];
  Handle.show handle;
  [%expect {| ((requested Dark) (resolved Dark)) |}];
  Handle.do_actions handle [ Toggle ];
  Handle.show handle;
  [%expect {| ((requested Light) (resolved Light)) |}]
;;

let%expect_test "a provided state wins over the fallback" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  let component (local_ graph) =
    let requested, set_requested =
      Bonsai.state Bonsai_web_css_color_scheme.Requested_mode.System graph
    in
    let state =
      let%arr requested and set_requested in
      (* This state resolves [System] to [Dark] — the opposite of the fallback in a test
         environment — so output proves the provided state is answering. *)
      let resolved_mode : Bonsai_web_css_color_scheme.Resolved_mode.t =
        match requested with
        | Light -> Light
        | Dark | System -> Dark
      in
      { Bonsai_web_css_color_scheme.Expert.State.requested_mode = requested
      ; resolved_mode
      ; set_requested_mode = set_requested
      }
    in
    Bonsai_web_css_color_scheme.Expert.provide state ~inside:getters_and_setters graph
  in
  let handle = Handle.create (module Spec) component in
  Handle.show handle;
  [%expect {| ((requested System) (resolved Dark)) |}];
  (* The setter routes to the provided state, not the fallback's. *)
  Handle.do_actions handle [ Set Light ];
  Handle.show handle;
  [%expect {| ((requested Light) (resolved Light)) |}];
  (* And the fallback's persisted state was untouched by that set. *)
  let fallback_handle = Handle.create (module Spec) installed in
  Handle.show fallback_handle;
  [%expect {| ((requested System) (resolved Light)) |}]
;;

let%expect_test "install: provides the fallback state with a configurable default" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  let app (local_ graph) =
    let%arr { Spec.view; _ } = getters_and_setters graph in
    Vdom.Node.text (Sexp.to_string_hum [%sexp (view : View.t)])
  in
  let handle =
    Handle.create
      (Result_spec.vdom Fn.id)
      (Bonsai_web_css_color_scheme.install ~default:Dark app)
  in
  Handle.show handle;
  [%expect {| ((requested Dark) (resolved Dark)) |}]
;;

let%expect_test "install: an explicit user choice survives a different default" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  (* The user explicitly picks [System], rather than never having chosen. *)
  let handle = Handle.create (module Spec) installed in
  Handle.do_actions handle [ Set System ];
  Handle.show handle;
  [%expect {| ((requested System) (resolved Light)) |}];
  (* A later [install] with a different [default] honors that persisted choice instead of
     applying [Dark]. *)
  let handle =
    Handle.create
      (module Spec)
      (Bonsai_web_css_color_scheme.install ~default:Dark getters_and_setters)
  in
  Handle.show handle;
  [%expect {| ((requested System) (resolved Light)) |}]
;;

(* This test must stay last: raising mid-stabilization leaves Bonsai's shared incremental
   state poisoned, so any test that runs afterwards in this executable would fail. *)
let%expect_test "the getters and setters raise without an installed state" =
  Bonsai_web_css_color_scheme.For_testing.reset_persisted_state ();
  Expect_test_helpers_core.require_does_raise (fun () ->
    let handle = Handle.create (module Spec) getters_and_setters in
    Handle.show handle);
  [%expect
    {| "[Bonsai_web_css_color_scheme] was used without an installed state. Wrap your top-level computation in [Bonsai_web_css_color_scheme.install], or provide a state via [Bonsai_web_css_color_scheme.Expert.provide]." |}]
;;
