open! Core

type t =
  { error : string
  ; position : Position.t
  }
[@@deriving fields ~getters, sexp_of, compare]

let of_error error ~position = { error = Error.to_string_hum error; position }
let compare_positions { position = p1; _ } { position = p2; _ } = Position.compare p1 p2

module Line_containing_nth_char = struct
  type t =
    { line_index : int
    ; char_index_in_line : int
    }

  let compute lines n =
    let rec find_line lines n =
      match lines with
      | [] -> None
      | (line_index, line) :: rest ->
        let line_length = String.length line + 1 in
        if n < line_length
        then Some { char_index_in_line = n; line_index }
        else find_line rest (n - line_length)
    in
    find_line (List.mapi lines ~f:(fun i x -> i, x)) n
  ;;
end

let to_string_referencing_input
  ~input_string
  { error; position = { offset_start; offset_end }; _ }
  =
  let all_lines = String.split_lines input_string in
  let start_line = Line_containing_nth_char.compute all_lines offset_start in
  let end_line = Line_containing_nth_char.compute all_lines (offset_end - 1) in
  match Option.both start_line end_line with
  | None -> error
  | Some
      ( { line_index = start_li; char_index_in_line = start_ci }
      , { line_index = end_li; char_index_in_line = end_ci } ) ->
    List.slice all_lines start_li (end_li + 1)
    |> List.concat_mapi ~f:(fun i line ->
      let prefix = sprintf "%d| " (i + start_li + 1) in
      let offset = String.length prefix in
      let pointer_start = if i = 0 then start_ci else 0 in
      let pointer_end =
        if i = end_li - start_li then end_ci + 1 else String.length line
      in
      let pointer_line =
        String.make (offset + pointer_start) ' '
        ^ String.make (pointer_end - pointer_start) '^'
      in
      [ prefix ^ line; pointer_line ])
    |> fun error_lines -> String.concat (error :: error_lines) ~sep:"\n"
;;

let to_string { error; position = { offset_start; offset_end } } =
  [%string "[%{offset_start#Int},%{offset_end#Int}]: %{error}"]
;;
