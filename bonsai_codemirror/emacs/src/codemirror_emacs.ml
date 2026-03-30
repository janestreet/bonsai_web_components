open! Core
open Js_of_ocaml

let create () : Codemirror.State.Extension.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr {|function(){ return codemirror_emacs__emacs(); }|})
    [||]
;;
