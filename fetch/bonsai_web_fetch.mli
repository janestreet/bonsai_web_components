open! Core
open! Async_kernel

module Method : sig
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
end

module Credentials : sig
  type t =
    | Omit
    | Same_origin
    | Include
  [@@deriving sexp_of]
end

module Mode : sig
  type t =
    | Same_origin
    | No_cors
    | Cors
    | Navigate
    | Websocket
  [@@deriving sexp_of]
end

module Response : sig
  type t

  val ok : t -> bool
  val status : t -> int
  val url : t -> string
  val redirected : t -> bool
  val headers : t -> string String.Map.t
  val body : t -> string Or_error.t Ui_effect.t
  val body_bytes : t -> bytes Or_error.t Ui_effect.t
  val body_async : t -> string Or_error.t Deferred.t
  val body_bytes_async : t -> bytes Or_error.t Deferred.t
end

(** [fetch url] send an HTTP request to the specified [url].

    See https://developer.mozilla.org/en-US/docs/Web/API/fetch for detailed docs. *)
val fetch
  :  ?meth:Method.t
  -> ?headers:string String.Map.t
  -> ?body:string
  -> ?credentials:Credentials.t
  -> ?mode:Mode.t
  -> ?keepalive:bool
  -> string
  -> Response.t Or_error.t Ui_effect.t

(** Like [fetch] but returns an async deferred instead of an effect. *)
val fetch_async
  :  ?meth:Method.t
  -> ?headers:string String.Map.t
  -> ?body:string
  -> ?credentials:Credentials.t
  -> ?mode:Mode.t
  -> ?keepalive:bool
  -> string
  -> Response.t Or_error.t Deferred.t
