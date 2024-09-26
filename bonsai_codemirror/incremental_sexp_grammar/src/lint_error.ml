open! Core
module Token = Tokenizer.Token

module Contents = struct
  type t =
    | Tokens of Tokens_for_error.t
    | Single_position of int
  [@@deriving sexp_of, compare]
end

type t =
  { error_description : string
  ; corrections : Correction.t list
  ; contents : Contents.t
  }
[@@deriving fields ~getters, sexp_of, compare]

let compare_error_descriptions { error_description = a; _ } { error_description = b; _ } =
  String.compare a b
;;

let to_syntax_error { error_description; corrections; contents } =
  let error =
    let value_description =
      match contents with
      | Tokens tokens -> Some [%string "Parsed: `%{Tokens_for_error.to_string tokens}`."]
      | Single_position _ -> None
    in
    let correction_text_opt = Correction.list_to_string corrections in
    [ Some error_description; value_description; correction_text_opt ]
    |> List.filter_opt
    |> String.concat_lines
  in
  let position =
    match contents with
    | Tokens tokens ->
      { Position.offset_start = Tokens_for_error.offset_start tokens
      ; Position.offset_end = Tokens_for_error.offset_end tokens
      }
    | Single_position position ->
      { Position.offset_start = position; Position.offset_end = position + 1 }
  in
  { Syntax_error.error; position }
;;

let of_tokens ?(corrections = []) error_description tokens =
  { error_description; corrections; contents = Tokens tokens }
;;

let at_pos ?(corrections = []) error_description ~pos =
  { error_description; corrections; contents = Single_position pos }
;;

let at_pos_of_first_token ?(corrections = []) error_description tokens =
  at_pos ~corrections error_description ~pos:(Tokens_for_error.offset_start tokens)
;;
