open! Core
open Js_of_ocaml

let create ?(use_system_clipboard = false) () : Codemirror.State.Extension.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr
       {|function(use_system_clipboard){ return codemirror_vim__vim({ use_system_clipboard : use_system_clipboard }); }|})
    [| Js.Unsafe.inject (Js.bool use_system_clipboard) |]
;;

let enter_insert_mode view =
  (* The vim plugin doesn't expose the enterInsertMode action, so we have to do it this
     way by simulating the keypress *)
  let handle_key view key =
    Js.Unsafe.fun_call
      (Js.Unsafe.pure_js_expr
         {|function(view, key) {
           return view.cm.state.vimPlugin.handleKey({
             key: key,
             preventDefault: function() {},
             stopPropagation: function() {}
           }, view);
         }|})
      [| Js.Unsafe.inject view; Js.Unsafe.inject (Js.string key) |]
  in
  handle_key view "i"
;;

let keymap_to_args ~mode ~from ~to_ =
  let from_param = from |> Js.string |> Js.Unsafe.coerce in
  let to_param = to_ |> Js.string |> Js.Unsafe.coerce in
  match mode with
  | `Normal -> [| from_param; to_param |]
  | `Insert -> [| from_param; to_param; Js.string "insert" |> Js.Unsafe.coerce |]
  | `Visual -> [| from_param; to_param; Js.string "visual" |> Js.Unsafe.coerce |]
;;

let map ?(mode = `Normal) ~from ~to_ () =
  keymap_to_args ~mode ~from ~to_
  |> Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.map |})
;;

let noremap ?(mode = `Normal) ~from ~to_ () =
  keymap_to_args ~mode ~from ~to_
  |> Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.noremap |})
;;

let mapclear () =
  Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.mapclear |}) [||]
;;

let define_ex ~name ~prefix func =
  let name_param = name |> Js.string |> Js.Unsafe.coerce in
  let prefix_param = prefix |> Js.string |> Js.Unsafe.coerce in
  let func_param = Js.wrap_callback func |> Js.Unsafe.inject in
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr {|codemirror_vim__Vim.defineEx |})
    [| name_param; prefix_param; func_param |]
;;
