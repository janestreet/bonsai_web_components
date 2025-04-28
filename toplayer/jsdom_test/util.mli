open! Core
open Jsdom
module Handle := Handle_experimental

(** These functions are used to test popover state. [selector] defaults to "[popover]".
    This works for modals too, because we implement them on top of popovers. *)
val assert_open_but_not_shown
  :  ?here:Stdlib.Lexing.position
  -> ?selector:string
  -> _ Handle.t
  -> unit

val assert_open_and_shown
  :  ?here:Stdlib.Lexing.position
  -> ?selector:string
  -> _ Handle.t
  -> unit

val assert_not_in_dom
  :  ?here:Stdlib.Lexing.position
  -> ?selector:string
  -> _ Handle.t
  -> unit
