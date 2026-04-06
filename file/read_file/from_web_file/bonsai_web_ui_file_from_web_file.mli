(** A library for creating and driving a [Bonsai_web_ui_file.t] using the Web File API.

    You need a [File.file] to get started here. The Web File API does not allow you to
    create an arbitrary such object -- i.e. javascript programs are not permitted to just
    read arbitrary files off the user's disk. The most common way of creating a file is to
    use a file picker form to allow the user to select a file.

    In other words, most users should use [Bonsai_web_ui_form.Elements.File_picker]
    instead of this module directly.

    [?mode] defaults to [`Raw_contents] which will give you the content of the file
    directly, but [`As_data_url] is also available if you would prefer to have the file
    read in a form that can be immediately used as a URL in e.g. <img> tags. *)

open! Core
open Js_of_ocaml

val create
  :  ?mode:[ `Raw_contents | `As_data_url ]
  -> File.file Js.t
  -> Bonsai_web_ui_file.t
