open! Core
open Virtual_dom
module Codemirror = Codemirror_bindings

module Language : sig
  type t =
    | Plaintext
    | OCaml
    | Diff
    | Html
    | Css
    | Python
    | Common_lisp
    | Sexp
    | Scheme
    | Sql
    | Javascript
    | Markdown
    | Php
    | Rust
    | Xml
    | FSharp
end

module Theme : sig
  type t =
    | Basic_dark
    | Basic_light
    | Gruvbox_dark
    | Nord
    | Solarized_dark
    | Solarized_light
    | Material_dark
    | Vscode_dark
    | Vscode_light
    | Vscode_default
end

val make
  :  ?extension:Codemirror.State.Extension.t
  -> ?line_numbers:bool
  -> ?line_wrapping:bool
  -> ?print_full_document:bool
       (** This option may impact performance but enables the browser's Ctrl+F search
           functionality. *)
  -> ?on_line_number_click:(int -> unit Ui_effect.t)
  -> ?scroll_to:int
  -> language:Language.t
  -> theme:Theme.t
  -> string
  -> Vdom.Node.t
