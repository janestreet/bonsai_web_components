open! Core

let create extensions =
  Codemirror.State.Editor_state.create
    (Codemirror.State.Editor_state_config.create
       ~extensions:(Codemirror.Basic_setup.minimal_setup :: extensions)
       ())
;;

let empty = create []
