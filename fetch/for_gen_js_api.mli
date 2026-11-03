open! Js_of_ocaml
open! Gen_js_api

module Error : sig
  type t

  val name : t -> string [@@js.get]
  val message : t -> string [@@js.get]
end

module Promise : sig
  type t

  (* _ Sadly we need to have [Ojs.t]s here to force us to do the right conversion if we
     expect back e.g. an ocaml string. (This could be addressed by further wrapping this
     such that we include the conversion functions, but since this is internal to this
     module the current thing seems fine.) *)

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
  val then_ : t -> f:(Ojs.t -> Ojs.t) -> t [@@js.call]
  val catch : t -> f:(Error.t -> unit) -> t [@@js.call]
end

module Headers : sig
  type t

  val create : unit -> t [@@js.new "Headers"]
  val set : t -> name:string -> value:string -> unit [@@js.call]
  val get : t -> string -> string [@@js.call]
  val for_each : t -> (value:string -> name:string -> unit) -> unit [@@js.call]
end

module Options : sig
  type t

  val create
    :  ?method_:string
    -> ?headers:Headers.t
    -> ?body:string
    -> ?credentials:string
    -> ?mode:string
    -> ?keepalive:bool
    -> unit
    -> t
  [@@js.builder]
end

module Response : sig
  type t

  val t_of_js : Ojs.t -> t
  val t_to_js : t -> Ojs.t
  val ok : t -> bool [@@js.get]
  val status : t -> int [@@js.get]
  val headers : t -> Headers.t [@@js.get]
  val text : t -> Promise.t [@@js.call]
  val bytes : t -> Promise.t [@@js.call]
  val url : t -> string [@@js.get]
  val redirected : t -> bool [@@js.get]
end

val fetch : string -> Options.t -> Promise.t [@@js.global]
