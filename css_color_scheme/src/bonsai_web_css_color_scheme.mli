open! Core
open! Bonsai_web

(** Bonsai state for CSS [color-scheme]. Use [install] for the built-in
    LocalStorage-backed state, or [Expert.provide] to plug in app-owned state. *)

module Resolved_mode : sig
  type t =
    | Light
    | Dark
  [@@deriving sexp, compare, equal, enumerate]
end

module Requested_mode : sig
  type t =
    | Light
    | Dark
    | System
  [@@deriving sexp, compare, equal, enumerate]
end

val requested_mode : Bonsai.graph @ local -> Requested_mode.t Bonsai.t
val resolved_mode : Bonsai.graph @ local -> Resolved_mode.t Bonsai.t

val set_requested_mode
  :  Bonsai.graph @ local
  -> (Requested_mode.t -> unit Effect.t) Bonsai.t

(** Toggle between the resolved light/dark modes. *)
val toggle : Bonsai.graph @ local -> unit Effect.t Bonsai.t

(** Installs the built-in state and writes [color-scheme] to the document element. *)
val install
  :  ?default:Requested_mode.t
  -> (Bonsai.graph @ local -> 'a Bonsai.t)
  -> Bonsai.graph @ local
  -> 'a Bonsai.t

module Expert : sig
  module State : sig
    type t =
      { requested_mode : Requested_mode.t
      ; resolved_mode : Resolved_mode.t
      ; set_requested_mode : Requested_mode.t -> unit Effect.t
      }
  end

  (** Provide app-owned state. Unlike [install], this does not write the document's
      [color-scheme] property; the caller is expected to keep [color-scheme] in sync with
      [resolved_mode] themselves (e.g. Skyline writes it from its theme). *)
  val provide
    :  State.t Bonsai.t
    -> inside:(Bonsai.graph @ local -> 'a Bonsai.t)
    -> Bonsai.graph @ local
    -> 'a Bonsai.t
end

module For_testing : sig
  (** Resets the library-owned fallback state persistence, as if the user had never
      expressed a preference. Tests that exercise the fallback state should call this
      first so they don't depend on each other's writes. *)
  val reset_persisted_state : unit -> unit
end
