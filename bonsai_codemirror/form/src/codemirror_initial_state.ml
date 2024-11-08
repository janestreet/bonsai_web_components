open! Core

let empty =
  Codemirror.State.Editor_state.create
    (Codemirror.State.Editor_state_config.create
       ~extensions:[ Codemirror.Basic_setup.minimal_setup ]
       ())
;;
