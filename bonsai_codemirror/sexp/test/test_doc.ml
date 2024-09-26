open! Core
module Doc = Codemirror_sexp_for_testing.Doc

let s =
  {|
Hello!

I am a string
with multiple lines


sometimes there's multiple spaces in between!
I hope you have a wonderful day.|}
  |> Doc.create
;;

let rec iter_until_done iterator =
  match iterator with
  | Doc.Line_iterator.Done -> ()
  | In_progress (next, line) ->
    print_s [%message (line : string)];
    iter_until_done (Doc.Line_iterator.next next)
;;

let%expect_test "starting at 0" =
  Doc.iter_lines s ~start_line:0 |> iter_until_done;
  [%expect
    {|
    (line "")
    (line Hello!)
    (line "")
    (line "I am a string")
    (line "with multiple lines")
    (line "")
    (line "")
    (line "sometimes there's multiple spaces in between!")
    (line "I hope you have a wonderful day.")
    |}]
;;

let%expect_test "starting at 2" =
  Doc.iter_lines s ~start_line:2 |> iter_until_done;
  (* The first line is a newline,  *)
  [%expect
    {|
    (line "")
    (line "I am a string")
    (line "with multiple lines")
    (line "")
    (line "")
    (line "sometimes there's multiple spaces in between!")
    (line "I hope you have a wonderful day.")
    |}]
;;

let%expect_test "starting at 4" =
  Doc.iter_lines s ~start_line:4 |> iter_until_done;
  [%expect
    {|
    (line "with multiple lines")
    (line "")
    (line "")
    (line "sometimes there's multiple spaces in between!")
    (line "I hope you have a wonderful day.")
    |}]
;;

let%expect_test "starting at 6" =
  Doc.iter_lines s ~start_line:6 |> iter_until_done;
  [%expect
    {|
    (line "")
    (line "sometimes there's multiple spaces in between!")
    (line "I hope you have a wonderful day.")
    |}]
;;

let%expect_test "starting at 8 (last line)" =
  Doc.iter_lines s ~start_line:8 |> iter_until_done;
  [%expect {| (line "I hope you have a wonderful day.") |}]
;;

let%expect_test "starting at 9 (past end)" =
  Doc.iter_lines s ~start_line:9 |> iter_until_done;
  [%expect {| |}]
;;
