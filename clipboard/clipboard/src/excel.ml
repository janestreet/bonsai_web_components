module Internal_formatter = Formatter
open Core

let quote = '"'
let sep = '\t'
let newline = '\n'

module Formatter = struct
  let data_type = Datatype.Text_plain

  let rows_to_output ?headers:_ rows =
    let count = List.length rows in
    if count = 0
    then ""
    else (
      let first i = i = 0 in
      let buf = Buffer.create (16 * count) in
      List.iteri rows ~f:(fun i row ->
        if not (first i) then Buffer.add_char buf newline;
        Buffer.add_string
          buf
          (* The following call also escapes necessary characters *)
          (Delimited_kernel.Write.By_row.line_to_string ~quote ~sep row));
      Buffer.contents buf)
  ;;

  let create () = Internal_formatter.create data_type rows_to_output

  let%expect_test "format rows with special characters" =
    let rows = [ [ "a"; "b\t\"\nc" ]; [ "d"; "e" ] ] in
    printf "%s" (rows_to_output rows);
    [%expect
      {|
      a	"b	""
      c"
      d	e
      |}]
  ;;
end

module Parser = struct
  let data_type = Datatype.Text_plain

  let rows_of_input str =
    let rows =
      Delimited_kernel.Read.list_of_string
        ~sep
        ~header:`No
        Delimited_kernel.Read.Row.builder
        str
    in
    List.map rows ~f:(fun r -> Delimited_kernel.Read.Row.to_list r)
  ;;

  let create () = Parser.create data_type rows_of_input

  let%expect_test "parse rows with special characters" =
    let rows = [ [ "a"; "b\t\"\nc" ]; [ "d"; "e" ] ] in
    let text = Formatter.rows_to_output rows in
    let parsed = rows_of_input text in
    printf !"%{sexp:string list list}" parsed;
    [%expect
      {|
      ((a  "b\t\"\
          \nc") (d e))
      |}]
  ;;
end

let formatter = Formatter.create ()
let parser = Parser.create ()
