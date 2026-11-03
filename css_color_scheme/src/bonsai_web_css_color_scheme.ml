open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

module Resolved_mode = struct
  type t =
    | Light
    | Dark
  [@@deriving sexp, compare, equal, enumerate]
end

module Requested_mode = struct
  type t =
    | Light
    | Dark
    | System
  [@@deriving sexp, compare, equal, enumerate]
end

module State = struct
  type t =
    { requested_mode : Requested_mode.t
    ; resolved_mode : Resolved_mode.t
    ; set_requested_mode : Requested_mode.t -> unit Effect.t
    }
end

let am_in_browser = Am_running_how_js.(am_in_browser am_running_how)

module Fallback = struct
  (* The user's persisted choice. [None] means they have never expressed a preference, in
     which case the default requested mode (configurable via [install]) applies. An
     explicit choice of [System] is stored as [Some System] so that it survives a later
     change to the installed default. *)
  let requested_var =
    Persistent_var.create
      (module struct
        type t = Requested_mode.t option [@@deriving sexp]
      end)
      `Local_storage
      ~unique_id:"bonsai_web_css_color_scheme.requested_mode.badfc8aa"
      ~default:None
  ;;

  (* Lazy so that merely linking this library doesn't install a media-query listener.
     Forced at graph-build time by [state]. *)
  let system_resolved_mode : Resolved_mode.t Bonsai.t Lazy.t =
    lazy
      (let mode_of_matches matches : Resolved_mode.t =
         match Js.to_bool matches with
         | true -> Dark
         | false -> Light
       in
       match Browser_expert.Global.window () with
       | None -> Bonsai.return Resolved_mode.Light
       | Some window ->
         let media_query_list =
           window##matchMedia ~query:(Js.string "(prefers-color-scheme: dark)")
         in
         let var =
           Bonsai.Expert.Var.create (mode_of_matches media_query_list##.matches)
         in
         let (_ : unit) =
           media_query_list##addListener
             ~listener:
               (Js.some
                  (Js.wrap_meth_callback (fun _this ~event:_ ->
                     Bonsai.Expert.Var.set
                       var
                       (mode_of_matches media_query_list##.matches))))
         in
         Bonsai.Expert.Var.value var)
  ;;

  let state ~default : State.t Bonsai.t =
    let system_resolved_mode = force system_resolved_mode in
    let requested = Persistent_var.value requested_var in
    let%arr requested and system_resolved_mode in
    let requested_mode = Option.value requested ~default in
    let resolved_mode : Resolved_mode.t =
      match requested_mode with
      | Light -> Light
      | Dark -> Dark
      | System -> system_resolved_mode
    in
    { State.requested_mode
    ; resolved_mode
    ; set_requested_mode =
        (fun requested_mode -> Persistent_var.effect requested_var (Some requested_mode))
    }
  ;;
end

let scope : State.t option Bonsai.Dynamic_scope.t =
  Bonsai.Dynamic_scope.create ~name:"bonsai_web_css_color_scheme.state" ~fallback:None ()
;;

let state (local_ graph) : State.t Bonsai.t =
  let%arr provided = Bonsai.Dynamic_scope.lookup scope graph in
  match provided with
  | Some state -> state
  | None ->
    raise_s
      [%message
        "[Bonsai_web_css_color_scheme] was used without an installed state. Wrap your \
         top-level computation in [Bonsai_web_css_color_scheme.install], or provide a \
         state via [Bonsai_web_css_color_scheme.Expert.provide]."]
;;

let requested_mode (local_ graph) =
  let%arr { State.requested_mode; _ } = state graph in
  requested_mode
;;

let resolved_mode (local_ graph) =
  let%arr { State.resolved_mode; _ } = state graph in
  resolved_mode
;;

let set_requested_mode (local_ graph) =
  let%arr { State.set_requested_mode; _ } = state graph in
  set_requested_mode
;;

let toggle (local_ graph) =
  let%arr { State.resolved_mode; set_requested_mode; _ } = state graph in
  match resolved_mode with
  | Dark -> set_requested_mode Light
  | Light -> set_requested_mode Dark
;;

let set_color_scheme_on_document_element =
  Effect.of_sync_fun (fun (resolved_mode : Resolved_mode.t) ->
    match am_in_browser with
    | false -> ()
    | true ->
      let value =
        match resolved_mode with
        | Light -> "light"
        | Dark -> "dark"
      in
      let (_ : _ Js.t) =
        Dom_html.document##.documentElement##.style##setProperty
          (Js.string "color-scheme")
          (Js.string value)
          Js.undefined
      in
      ())
;;

let set_document_color_scheme_on_change resolved_mode (local_ graph) =
  Bonsai.Edge.on_change
    ~equal:Resolved_mode.equal
    resolved_mode
    ~callback:(Bonsai.return set_color_scheme_on_document_element)
    graph
;;

module Expert = struct
  module State = State

  let provide state ~inside (local_ graph) =
    let state =
      let%arr state in
      Some state
    in
    Bonsai.Dynamic_scope.set scope state ~inside graph
  ;;
end

let install ?(default = Requested_mode.System) app (local_ graph) =
  Expert.provide
    (Fallback.state ~default)
    ~inside:(fun (local_ graph) ->
      set_document_color_scheme_on_change (resolved_mode graph) graph;
      app graph)
    graph
;;

module For_testing = struct
  let reset_persisted_state () =
    Persistent_var.set Fallback.requested_var None;
    Persistent_var.clear_persistence Fallback.requested_var
  ;;
end
