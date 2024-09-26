open! Core
open Virtual_dom
open Js_of_ocaml

module Bindings = struct
  class type data_transfer_item = object
    method kind : Js.js_string Js.t Js.readonly_prop
    method name : Js.js_string Js.t Js.readonly_prop
    method getAsFile : File.file Js.t Js.meth
  end

  class type data_transfer = object
    inherit Dom_html.dataTransfer
    method items : data_transfer_item Js.t Js.js_array Js.t Js.optdef Js.readonly_prop
  end
end

let attr
  ?mime_types
  ?(on_drag_over = Vdom.Effect.Ignore)
  ?(on_drag_leave = Vdom.Effect.Ignore)
  on_file_upload
  =
  let drag_over =
    Vdom.Attr.on_dragover (fun _event ->
      Vdom.Effect.Many [ Vdom.Effect.Prevent_default; on_drag_over ])
  in
  let on_drag_leave_attr =
    match on_drag_leave with
    | Vdom.Effect.Ignore -> Vdom.Attr.empty
    | other_effect -> Vdom.Attr.on_dragleave (fun _ -> other_effect)
  in
  let drop =
    Vdom.Attr.on_drop (fun event ->
      let data_transfer : Bindings.data_transfer Js.t = Obj.magic event##.dataTransfer in
      let respond_with =
        (* this code is translated from https://github.com/mdn/dom-examples/blob/main/drag-and-drop/File-drag.html *)
        match Js.Optdef.to_option data_transfer##.items with
        | Some items ->
          let out = Js.array [||] in
          for i = 0 to items##.length - 1 do
            match Js.Optdef.to_option (Js.array_get items i) with
            | Some item ->
              if phys_equal item##.kind (Js.string "file")
              then ignore (out##push item##getAsFile : int)
              else ()
            | None -> ()
          done;
          Array.to_list (Js.to_array out)
        | None ->
          let items = data_transfer##.files in
          let out = Js.array [||] in
          for i = 0 to items##.length - 1 do
            match Js.Opt.to_option (items##item i) with
            | Some item -> (ignore : int -> unit) (out##push item)
            | None -> ()
          done;
          Array.to_list (Js.to_array out)
      in
      let good_files, files_not_matching_mime_type =
        match mime_types with
        | None -> respond_with, []
        | Some mime_types ->
          List.partition_tf respond_with ~f:(fun file ->
            List.mem mime_types ~equal:String.equal (Js.to_string file##._type))
      in
      Vdom.Effect.Many
        [ on_drag_leave
        ; on_file_upload good_files ~files_not_matching_mime_type
        ; Vdom.Effect.Prevent_default
        ])
  in
  Vdom.Attr.many [ drag_over; drop; on_drag_leave_attr ]
;;
