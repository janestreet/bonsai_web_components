open! Core
open Js_of_ocaml
open Virtual_dom
open Clipboard

module Vdom_attr = struct
  let event_stop_and_prevent =
    Vdom.Effect.Many [ Vdom.Effect.Stop_propagation; Vdom.Effect.Prevent_default ]
  ;;

  let on_copy f = Vdom.Attr.on_copy f
  let on_cut f = Vdom.Attr.on_cut f
  let on_paste f = Vdom.Attr.on_paste f

  let intercept_copy_type type_ f =
    on_copy (fun event ->
      event##.clipboardData##setData (Js.string type_) (Js.string (f ()));
      event_stop_and_prevent)
  ;;

  let intercept_copy_plain f =
    intercept_copy_type (Datatype.to_string Datatype.Text_plain) f
  ;;

  let intercept_cut_type type_ f =
    on_cut (fun event ->
      let text, evt = f () in
      event##.clipboardData##setData (Js.string type_) (Js.string text);
      Vdom.Effect.Many [ evt; event_stop_and_prevent ])
  ;;

  let intercept_cut_plain f =
    intercept_cut_type (Datatype.to_string Datatype.Text_plain) f
  ;;

  let intercept_paste_type type_ f =
    on_paste (fun event ->
      let text = Js.to_string (event##.clipboardData##getData (Js.string type_)) in
      let event = f text in
      Vdom.Effect.Many [ event; event_stop_and_prevent ])
  ;;

  let intercept_paste_plain f =
    intercept_paste_type (Datatype.to_string Datatype.Text_plain) f
  ;;

  let set_copy_content js_event map =
    let cl = js_event##.clipboardData in
    Map.iteri map ~f:(fun ~key:t ~data:d ->
      let type_ = Datatype.to_string t in
      cl##setData (Js.string type_) (Js.string d))
  ;;

  let get_paste_content js_event =
    let cl = js_event##.clipboardData in
    let types = cl##.types in
    let len = types##.length in
    List.init len ~f:(fun i -> Js.array_get types i)
    |> List.map ~f:Js.Optdef.to_option
    |> List.filter_opt
    |> List.map ~f:(fun t -> Js.to_string t, cl##getData t)
    |> List.map ~f:(fun (t, d) -> Datatype.of_string t, Js.to_string d)
    |> Datatype.Map.of_alist_exn
  ;;

  let intercept_copy f =
    on_copy (fun js_event ->
      let map, event = f () in
      set_copy_content js_event map;
      Vdom.Effect.Many [ event_stop_and_prevent; event ])
  ;;

  let intercept_cut f =
    on_cut (fun js_event ->
      let map, event = f () in
      set_copy_content js_event map;
      Vdom.Effect.Many [ event_stop_and_prevent; event ])
  ;;

  let intercept_paste f =
    on_paste (fun js_event ->
      Vdom.Effect.Many [ event_stop_and_prevent; f (get_paste_content js_event) ])
  ;;

  let intercept_copy_opt f =
    on_copy (fun js_event ->
      match f () with
      | None -> Vdom.Effect.Ignore
      | Some (map, event) ->
        set_copy_content js_event map;
        Vdom.Effect.Many [ event_stop_and_prevent; event ])
  ;;

  let intercept_cut_opt f =
    on_cut (fun js_event ->
      match f () with
      | None -> Vdom.Effect.Ignore
      | Some (map, event) ->
        set_copy_content js_event map;
        Vdom.Effect.Many [ event_stop_and_prevent; event ])
  ;;

  let intercept_paste_opt f =
    on_paste (fun js_event ->
      match f (get_paste_content js_event) with
      | None -> Vdom.Effect.Ignore
      | Some event -> Vdom.Effect.Many [ event_stop_and_prevent; event ])
  ;;
end
