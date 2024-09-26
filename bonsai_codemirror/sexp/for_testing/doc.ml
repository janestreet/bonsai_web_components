open! Core

module Line_iterator = struct
  type in_progress = int * string array

  type t =
    | Done
    | In_progress of (in_progress * string)

  let next (i, lines) =
    if i >= Array.length lines
    then Done
    else (
      let line = lines.(i) in
      In_progress ((i + 1, lines), line))
  ;;
end

type t = string array

let create str = String.split_lines str |> Array.of_list
let to_lines t = Array.to_list t

let iter_lines lines ~start_line =
  if Array.length lines <= start_line
  then Line_iterator.Done
  else Line_iterator.next (start_line, lines)
;;
