open! Core
module Autocomplete = Autocomplete
module Rainbow_parentheses = Rainbow_parentheses
module Validation = Validation

let extension
  ?(enable_rainbow_parens = true)
  ?(enable_validation = true)
  ?(enable_autocomplete = false)
  grammar
  =
  [ Option.some_if enable_rainbow_parens (Rainbow_parentheses.extension ~grammar ())
  ; Option.some_if enable_validation (Validation.extension grammar)
  ; Option.some_if enable_autocomplete (Autocomplete.extension grammar)
  ]
  |> List.filter_opt
  |> Codemirror.State.Extension.of_list
;;
