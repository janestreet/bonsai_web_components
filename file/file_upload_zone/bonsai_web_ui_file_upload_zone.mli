open! Core
open Virtual_dom

(** Constructs an attribute that you can place on any element in order to
    turn that element into a drop-target for uploading files.


    - [?mime_types] is a list of mime-types that will be used as a filter on the
      files that the user is uploading.
    - [?on_drag_over] is scheduled when the user drags something over the node.
    - [?on_drag_leave] is scheduled when the drag leaves the node or when the drop
      is completed

    The main callback function is invoked when files are dropped on the element. The list
    can be empty if none of the files match the set of acceptable mime-types provided by
    the [mime_types] argument. *)
val attr
  :  ?mime_types:string list
  -> ?on_drag_over:unit Vdom.Effect.t
  -> ?on_drag_leave:unit Vdom.Effect.t
  -> (Js_of_ocaml.File.file Js_of_ocaml.Js.t list
      -> files_not_matching_mime_type:Js_of_ocaml.File.file Js_of_ocaml.Js.t list
      -> unit Vdom.Effect.t)
  -> Vdom.Attr.t
