open! Core
open! Bonsai_web
open Js_of_ocaml

(** [bonsai_web_drop_file] contains utilities for working with native OS files -- e.g.
    those from a file explorer program -- from web apps. *)

module Drop : sig
  type t =
    { drop_target : Vdom.Attr.t Bonsai.t
    ; dragging_over :
        [ `All_files_valid | `Some_files_valid | `No_files_valid ] option Bonsai.t
    }
end

(** [on_drop] Constructs an attribute that you can place on any element in order to
    turnthat element into a drop-target for uploading files. It also reports when files
    are being dragged over the drop target.

    - [?mime_types] is a list of mime-types that will be used as a filter on the files
      that the user is uploading.

    The [f] is invoked when files are dropped on the element. The list can be empty if
    none of the files match the set of acceptable mime-types provided by the [mime_types]
    argument. *)
val on_drop
  :  here:[%call_pos]
  -> ?mime_types:string list
  -> f:
       (File.file Js.t list
        -> files_not_matching_mime_type:File.file Js.t list
        -> unit Vdom.Effect.t)
         Bonsai.t
  -> local_ Bonsai.graph
  -> Drop.t

(** [on_paste] allows you to handle native OS files being pasted while the DOM element to
    which the returned [Vdom.Attr.t] is attached is focused. *)
val on_paste
  :  ?mime_types:string list
  -> (File.file Js.t list
      -> files_not_matching_mime_type:File.file Js.t list
      -> unit Vdom.Effect.t)
  -> Vdom.Attr.t

(** [read_file] will asynchronously read and return the contents of a DOM file.
    https://developer.mozilla.org/en-US/docs/Web/API/File *)
val read_file : File.file Js.t -> bytes Or_error.t Effect.t

module For_testing : sig
  module Bindings : sig
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
end
