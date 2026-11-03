open! Core
open Js_of_ocaml

let create ?(use_system_clipboard = false) () : Codemirror.State.Extension.t =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr
       {|function(use_system_clipboard){ return codemirror_vim__vim({ use_system_clipboard : use_system_clipboard }); }|})
    [| Js.Unsafe.inject (Js.bool use_system_clipboard) |]
;;

let is_insert_mode view =
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr
       {|function(view) {
           const cm = view.cm;
           const vim = cm && cm.state && cm.state.vim;
           return !!(vim && vim.insertMode);
         }|})
    [| Js.Unsafe.inject view |]
  |> Js.to_bool
;;

let on_insert_mode_change callback : Codemirror.State.Extension.t =
  let callback = Js.wrap_callback (fun is_insert -> callback (Js.to_bool is_insert)) in
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr
       {|function(callback) {
           return codemirror.View.ViewPlugin.define(function(view) {
             const cm = view.cm;
             let previous = null;

             function notify() {
               const current = !!(cm && cm.state.vim && cm.state.vim.insertMode);
               if (current !== previous) {
                 previous = current;
                 callback(current);
               }
             }

             if (cm && cm.on) cm.on("vim-mode-change", notify);
             notify();

             return {
               destroy() {
                 if (cm && cm.off) cm.off("vim-mode-change", notify);
               }
             };
           });
         }|})
    [| Js.Unsafe.inject callback |]
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

let unmap ?(mode = `Normal) ~from () =
  let from_param = from |> Js.string |> Js.Unsafe.coerce in
  let args =
    (* Note that the [`Normal] context must be expressed by passing no context argument:
       [unmap] only removes mappings whose context is exactly equal to the one passed, and
       default-keymap entries (and [map ~mode:`Normal] entries) have no context. *)
    match mode with
    | `Normal -> [| from_param |]
    | `Insert -> [| from_param; Js.string "insert" |> Js.Unsafe.coerce |]
    | `Visual -> [| from_param; Js.string "visual" |> Js.Unsafe.coerce |]
  in
  Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.unmap |}) args
;;

let mapclear () =
  Js.Unsafe.fun_call (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.mapclear |}) [||]
;;

let define_and_map_action ~keys ~name func =
  let () =
    Js.Unsafe.fun_call
      (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.defineAction |})
      [| name |> Js.string |> Js.Unsafe.coerce
       ; Js.wrap_callback (fun _cm _action_args -> func ()) |> Js.Unsafe.inject
      |]
  in
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr {| codemirror_vim__Vim.mapCommand |})
    [| keys |> Js.string |> Js.Unsafe.coerce
     ; Js.string "action" |> Js.Unsafe.coerce
     ; name |> Js.string |> Js.Unsafe.coerce
    |]
;;

let define_ex ~name ~prefix func =
  let name_param = name |> Js.string |> Js.Unsafe.coerce in
  let prefix_param = prefix |> Js.string |> Js.Unsafe.coerce in
  let func_param = Js.wrap_callback func |> Js.Unsafe.inject in
  Js.Unsafe.fun_call
    (Js.Unsafe.pure_js_expr {|codemirror_vim__Vim.defineEx |})
    [| name_param; prefix_param; func_param |]
;;
