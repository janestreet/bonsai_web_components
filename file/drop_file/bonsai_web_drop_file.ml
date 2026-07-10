open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open Js_of_ocaml

let contains (node : Dom_html.element Js.t) ~(descendant : Dom_html.element Js.t) =
  Js_of_ocaml.(
    Js.Unsafe.meth_call node "contains" [| Js.Unsafe.inject descendant |] |> Js.to_bool)
;;

module Bindings = struct
  class type data_transfer_item = object
    method kind : Js.js_string Js.t Js.readonly_prop
    method _type : Js.js_string Js.t Js.readonly_prop
    method getAsFile : File.file Js.t Js.opt Js.meth
  end

  class type data_transfer = object
    inherit Dom_html.dataTransfer
    method items : data_transfer_item Js.t Js.js_array Js.t Js.optdef Js.readonly_prop
  end
end

let data_transfer_files (data_transfer : Dom_html.dataTransfer Js.t) =
  let data_transfer : Bindings.data_transfer Js.t = Js.Unsafe.coerce data_transfer in
  (* this code is translated from
     https://github.com/mdn/dom-examples/blob/main/drag-and-drop/File-drag.html

     Crucially, we prefer [dataTransfer.items] to [dataTransfer.files], because
     [dataTransfer.files] is not accessible to [dragover] and [dragenter] events. We
     define both branches for type safety, but [dataTransfer.items] should always be
     present in modern browsers. *)
  match Js.Optdef.to_option data_transfer##.items with
  | Some items ->
    let out = Js.array [||] in
    for i = 0 to items##.length - 1 do
      match Js.Optdef.to_option (Js.array_get items i) with
      | Some item ->
        if Js.equals item##.kind (Js.string "file")
        then (
          let mime_type = Js.to_string item##._type in
          out##push (mime_type, Js.Opt.to_option item##getAsFile)
          |> (ignore : int -> unit))
        else ()
      | None -> ()
    done;
    Array.to_list (Js.to_array out)
  | None ->
    let items = data_transfer##.files in
    let out = Js.array [||] in
    for i = 0 to items##.length - 1 do
      match Js.Opt.to_option (items##item i) with
      | Some item ->
        (ignore : int -> unit) (out##push (Js.to_string item##._type, Some item))
      | None -> ()
    done;
    Array.to_list (Js.to_array out)
;;

let partition_valid_mime_type ?mime_types (files : (string * File.file Js.t option) list) =
  match mime_types with
  | None -> files, []
  | Some mime_types ->
    List.partition_tf files ~f:(fun (file_type, _file) ->
      List.mem mime_types ~equal:String.equal file_type)
;;

let dragging_over_state ?mime_types data_transfer =
  let good_files, files_not_matching_mime_type =
    data_transfer_files data_transfer |> partition_valid_mime_type ?mime_types
  in
  match good_files, files_not_matching_mime_type with
  | _ :: _, [] -> `All_files_valid
  | _ :: _, _ :: _ -> `Some_files_valid
  | [], _ :: _ | [], [] -> `No_files_valid
;;

(* File objects aren't always available in a data transfer object! For example, they won't
   be present during [dragenter], [dragover], and [dragleave] events. *)
let get_files_guaranteed_present_exn ?mime_types data_transfer ~f =
  let get_file (_file_name, file_opt) = Option.value_exn file_opt in
  let good_files, files_not_matching_mime_type =
    data_transfer_files data_transfer |> partition_valid_mime_type ?mime_types
  in
  f
    (List.map good_files ~f:get_file)
    ~files_not_matching_mime_type:(List.map files_not_matching_mime_type ~f:get_file)
;;

module Drop = struct
  type t =
    { drop_target : Vdom.Attr.t Bonsai.t
    ; dragging_over :
        [ `All_files_valid | `Some_files_valid | `No_files_valid ] option Bonsai.t
    }
end

let const_ignore () = Effect.Ignore

let on_drop ~(here : [%call_pos]) ?mime_types ~f (local_ graph) =
  let dom_refs_handle = Bonsai_web_low_level_vdom.Dom_ref.tracker graph in
  let dragging_over, set_dragging_over = Bonsai.state None graph in
  let drop_target =
    let%arr f and set_dragging_over and dom_refs_handle in
    Vdom.Attr.many
      [ dom_refs_handle.attr
      ; (* File drag + drop is more complicated than one might expect.

           For an element to be a valid drop target, the [dragenter], [dragover], and
           [drop] events all need to explicitly be handled:
           https://html.spec.whatwg.org/multipage/dnd.html#event-drag *)
        Vdom.Attr.on_dragenter (fun e ->
          Js.Opt.case e##.dataTransfer const_ignore (fun data_transfer ->
            (* We set to [copy], because uploading files to a web UI can't change them in
               the OS. *)
            data_transfer##.dropEffect := Js_of_ocaml.Js.string "copy";
            (* The [dragging_over_state] check must happen synchronously!!! *)
            let dragging_over = dragging_over_state ?mime_types data_transfer in
            Effect.Many
              [ (Effect.Prevent_default [@alert "-deprecated"])
              ; set_dragging_over (Some dragging_over)
              ]))
      ; Vdom.Attr.on_dragover (fun e ->
          Js.Opt.case e##.dataTransfer const_ignore (fun data_transfer ->
            data_transfer##.dropEffect := Js_of_ocaml.Js.string "copy";
            Effect.Prevent_default [@alert "-deprecated"]))
      ; Vdom.Attr.on_dragleave (fun e ->
          match Js.Opt.to_option e##.relatedTarget with
          | None -> set_dragging_over None
          | Some new_target ->
            let%bind.Effect dom_refs = dom_refs_handle.nodes in
            (* This awkwardness could go away if we stored the [e##.currentTarget]
               alongside dragging state on [dragenter], but putting mutable things like
               DOM nodes in [Bonsai.t]s is a bad idea. *)
            (match dom_refs with
             | [] ->
               Effect.print_s
                 [%message
                   "BUG: on_drop attr is active, but drop target DOM node wasn't found."
                     (here : Source_code_position.t)]
             | [ me ] ->
               if not (contains me ~descendant:new_target)
               then set_dragging_over None
               else Effect.Ignore
             | targets ->
               if not (List.exists targets ~f:(contains ~descendant:new_target))
               then set_dragging_over None
               else Effect.Ignore))
      ; Vdom.Attr.on_drop (fun e ->
          Js.Opt.case e##.dataTransfer const_ignore (fun data_transfer ->
            Effect.Many
              [ (Effect.Prevent_default [@alert "-deprecated"])
              ; get_files_guaranteed_present_exn ?mime_types data_transfer ~f
              ; set_dragging_over None
              ]))
      ]
  in
  { Drop.drop_target; dragging_over }
;;

let on_paste ?mime_types f =
  Vdom.Attr.on_paste (fun e ->
    Js.Opt.case e##.clipboardData const_ignore (fun clipboard_data ->
      get_files_guaranteed_present_exn ?mime_types clipboard_data ~f))
;;

let read_file (file : File.file Js.t) : bytes Or_error.t Effect.t =
  let reader = new%js File.fileReader in
  Ui_effect.Expert.of_fun ~f:(fun ~callback ~on_exn:_ ->
    reader##.onload
    := Dom.handler (fun _ ->
         let result =
           File.CoerceTo.arrayBuffer reader##.result
           |> Js.Opt.to_option
           |> Option.map ~f:Typed_array.Bytes.of_arrayBuffer
         in
         (match result with
          | None -> callback (Or_error.error_string "Failed loading image")
          | Some result -> callback (Ok result));
         Js._false);
    reader##.onerror
    := Dom.handler (fun _error ->
         let error : Js.error Js.t Js.opt = Obj.magic reader##.error in
         let error_message =
           match Js.Opt.to_option error with
           | None -> "Failed to read file"
           | Some error ->
             let msg = error##.message |> Js.to_string in
             [%string "Failed to read file: %{msg}"]
         in
         callback (Or_error.error_string error_message);
         Js._true);
    reader##readAsArrayBuffer file)
;;

module For_testing = struct
  module Bindings = Bindings
end
