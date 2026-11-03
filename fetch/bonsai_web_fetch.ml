open! Core
open! Async_kernel
open! Gen_js_api

module Method = struct
  type t =
    | GET
    | POST
    | PUT
    | DELETE
    | PATCH
    | OPTIONS
    | HEAD
    | TRACE
    | CONNECT
    | Other of string
  [@@deriving sexp_of]

  let to_string = function
    | GET -> "GET"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | PATCH -> "PATCH"
    | OPTIONS -> "OPTIONS"
    | HEAD -> "HEAD"
    | TRACE -> "TRACE"
    | CONNECT -> "CONNECT"
    | Other s -> s
  ;;
end

module Credentials = struct
  type t =
    | Omit
    | Same_origin
    | Include
  [@@deriving sexp_of]

  let to_string = function
    | Omit -> "omit"
    | Same_origin -> "same-origin"
    | Include -> "include"
  ;;
end

module Mode = struct
  type t =
    | Same_origin
    | No_cors
    | Cors
    | Navigate
    | Websocket
  [@@deriving sexp_of]

  let to_string = function
    | Same_origin -> "same-origin"
    | No_cors -> "no-cors"
    | Cors -> "cors"
    | Navigate -> "navigate"
    | Websocket -> "websocket"
  ;;
end

let js_to_error err =
  let name = For_gen_js_api.Error.name err in
  let message = For_gen_js_api.Error.message err in
  Or_error.error_string [%string "%{name}: %{message}"]
;;

module Response = struct
  type t = For_gen_js_api.Response.t

  let ok = For_gen_js_api.Response.ok
  let status = For_gen_js_api.Response.status
  let url = For_gen_js_api.Response.url
  let redirected = For_gen_js_api.Response.redirected

  let headers resp =
    let headers = For_gen_js_api.Response.headers resp in
    let map = ref String.Map.empty in
    For_gen_js_api.Headers.for_each headers (fun ~value ~name ->
      map := Map.set !map ~key:name ~data:value);
    !map
  ;;

  let body_async_handler resp ~get_body ~convert =
    let ivar = Ivar.create () in
    get_body resp
    |> For_gen_js_api.Promise.then_ ~f:(fun body ->
      convert body
      |> Result.of_option ~error:(Error.of_string "Response is not accessible")
      |> Ivar.fill_exn ivar
      |> [%js.of: unit])
    |> For_gen_js_api.Promise.catch ~f:(fun err -> Ivar.fill_exn ivar (js_to_error err))
    |> (ignore : For_gen_js_api.Promise.t -> unit);
    Ivar.read ivar
  ;;

  let body_async resp =
    body_async_handler
      resp
      ~get_body:For_gen_js_api.Response.text
      ~convert:[%js.to: string option]
  ;;

  let body_bytes_async resp =
    body_async_handler resp ~get_body:For_gen_js_api.Response.bytes ~convert:(fun body ->
      Ojs.option_of_js
        (fun typed_array ->
          (Obj.magic : Ojs.t -> Js_of_ocaml.Typed_array.uint8Array Js_of_ocaml.Js.t)
            typed_array
          |> Js_of_ocaml.Typed_array.Bytes.of_uint8Array)
        body)
  ;;

  let body = Ui_effect_of_deferred.of_deferred_fun body_async
  let body_bytes = Ui_effect_of_deferred.of_deferred_fun body_bytes_async
end

let fetch_async ?meth ?headers ?body ?credentials ?mode ?keepalive url =
  let options =
    let method_ = Option.map meth ~f:Method.to_string in
    let headers =
      Option.map headers ~f:(fun value_by_name ->
        let headers = For_gen_js_api.Headers.create () in
        Map.iteri value_by_name ~f:(fun ~key:name ~data:value ->
          For_gen_js_api.Headers.set headers ~name ~value);
        headers)
    in
    let credentials = Option.map credentials ~f:Credentials.to_string in
    let mode = Option.map mode ~f:Mode.to_string in
    For_gen_js_api.Options.create ?method_ ?headers ?body ?credentials ?mode ?keepalive ()
  in
  let ivar = Ivar.create () in
  For_gen_js_api.fetch url options
  |> For_gen_js_api.Promise.then_ ~f:(fun response ->
    let response = For_gen_js_api.Response.t_of_js response in
    Ivar.fill_exn ivar (Ok response) |> [%js.of: unit])
  |> For_gen_js_api.Promise.catch ~f:(fun err -> Ivar.fill_exn ivar (js_to_error err))
  |> (ignore : For_gen_js_api.Promise.t -> unit);
  Ivar.read ivar
;;

let fetch ?meth ?headers ?body ?credentials ?mode ?keepalive url =
  Ui_effect_of_deferred.of_deferred_fun
    (fetch_async ?meth ?headers ?body ?credentials ?mode ?keepalive)
    url
;;
