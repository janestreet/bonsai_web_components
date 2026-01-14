open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Expert = Byo_portal_private

let portals = Bonsai.Expert.Var.create String.Map.empty

module Action = struct
  type t =
    | Create of
        { initial_vdom : Vdom.Node.t
        ; parent : unit -> Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
        }
    | Apply_patch of Vdom.Node.t
    | Destroy
end

let apply_action_sync ~path action =
  Bonsai.Expert.Var.update portals ~f:(fun portals ->
    match action with
    | Action.Create { initial_vdom; parent } ->
      Map.update portals path ~f:(fun existing ->
        Option.iter existing ~f:Expert.destroy;
        Expert.create ~parent:(parent ()) initial_vdom)
    | Apply_patch vdom ->
      Map.change
        portals
        path
        ~f:(Option.map ~f:(fun portal -> Expert.apply_patch portal vdom))
    | Destroy ->
      (match Map.find portals path with
       | Some portal ->
         Expert.destroy portal;
         Map.remove portals path
       | None -> portals))
;;

let apply_action ~path action =
  Effect.of_thunk (fun () ->
    Virtual_dom.Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
      apply_action_sync ~path action))
;;

let component' ~parent vdom (local_ graph) =
  let path = Bonsai.path_id graph in
  let () =
    (* Create a new portal whenever the component is activated. *)
    let on_activate =
      let%arr vdom and path in
      apply_action ~path (Create { initial_vdom = vdom; parent })
    in
    (* Destroy any existing portal whenever the component is deactivated. *)
    let on_deactivate =
      let%arr path in
      apply_action ~path Destroy
    in
    Bonsai.Edge.lifecycle ~on_activate ~on_deactivate graph
  in
  (* Patch the portal contents whenever they change. *)
  let callback =
    let%arr path in
    fun vdom -> apply_action ~path (Apply_patch vdom)
  in
  Bonsai.Edge.on_change ~trigger:`After_display vdom ~equal:phys_equal ~callback graph
;;

let component ?(parent = Expert.global_toplayer_root) compute_vdom (local_ graph) =
  let (_ : unit Bonsai.t) =
    Bonsai.with_inverted_lifecycle_ordering
      ~compute_dep:compute_vdom
      ~f:(fun vdom graph ->
        component' ~parent vdom graph;
        return ())
      graph
  in
  ()
;;

let global_toplayer_root = Expert.global_toplayer_root
let ensure_global_toplayer_root_mounted = Expert.ensure_global_toplayer_root_mounted

module For_testing = struct
  let active_portals = portals
end
