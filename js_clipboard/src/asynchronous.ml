open! Core
open Js_of_ocaml
module Callback = Ui_effect.Private.Callback

let with_clipboard callback fn =
  Js.Optdef.case
    Js.Unsafe.global##.navigator##.clipboard
    (fun () ->
      ignore
        (Callback.respond_to
           callback
           (Or_error.error_string "No clipboard found. Check browser permissions.")
         : unit Ui_effect.t))
    fn
;;

let copy_text =
  let evaluator callback =
    let text = Callback.request callback in
    with_clipboard callback (fun clipboard ->
      (clipboard##writeText text)##then_
        (Js.wrap_callback (fun () -> Callback.respond_to callback (Ok ())))
        (Js.wrap_callback (fun err ->
           Callback.respond_to
             callback
             (Or_error.error_string (Js.to_string err##.message)))))
  in
  fun text -> Ui_effect.Private.make ~request:text ~evaluator
;;

let read_text =
  let evaluator callback =
    with_clipboard callback (fun clipboard ->
      clipboard##readText##then_
        (Js.wrap_callback (fun text -> Callback.respond_to callback (Ok text)))
        (Js.wrap_callback (fun err ->
           Callback.respond_to
             callback
             (Or_error.error_string (Js.to_string err##.message)))))
  in
  Ui_effect.Private.make ~request:() ~evaluator
;;

module ClipboardItem = struct
  class type clipboardItem = object end

  let create : (Js.Unsafe.any -> clipboardItem Js.t) Js.constr =
    Js.Unsafe.global##._ClipboardItem
  ;;
end

let copy_blob =
  let evaluator callback =
    let blobs = Callback.request callback in
    with_clipboard callback (fun clipboard ->
      let blobs =
        List.to_array blobs
        |> Array.map ~f:(fun blob -> blob##._type |> Js.to_string, Js.Unsafe.inject blob)
      in
      let item = new%js ClipboardItem.create (Js.Unsafe.obj blobs) in
      (clipboard##write (Js.array [| item |]))##then_
        (Js.wrap_callback (fun () -> Callback.respond_to callback (Ok ())))
        (Js.wrap_callback (fun err ->
           Callback.respond_to
             callback
             (Or_error.error_string (Js.to_string err##.message)))))
  in
  fun blobs -> Ui_effect.Private.make ~request:blobs ~evaluator
;;
