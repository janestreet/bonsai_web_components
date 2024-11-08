open! Core
open Codemirror_sexp_kernel.Parsing
open Codemirror_incremental_sexp_grammar
module Doc = Codemirror_sexp_for_testing.Doc

module Tester = struct
  type t = int list [@@deriving sexp_grammar]
end

module Handle = struct
  type t =
    { mutable doc : Doc.t
    ; mutable state : (Doc.t, Syntax_error.t) Grammared.t
    }

  let create s =
    let state =
      Grammared.create
        ~checkpoint_every_n_lines:2
        ~error_conv:Fn.id
        (module Doc)
        Tester.t_sexp_grammar.untyped
    in
    { doc = Doc.create s; state }
  ;;

  let parse_chunk t = t.state <- Grammared.apply_action t.state (Parse_some_lines t.doc)

  let update_doc t new_s =
    let new_lines = String.split_lines new_s in
    let first_line_changed_index =
      let same_len, _ = List.zip_with_remainder (Doc.to_lines t.doc) new_lines in
      match
        List.find_mapi same_len ~f:(fun i (a, b) ->
          if not (String.equal a b) then Some i else None)
      with
      | Some i -> i
      | None ->
        (* This is a pure append, so the first changed line is the one after the last
           current line. *)
        List.length same_len
    in
    t.doc <- String.concat_lines new_lines |> Doc.create;
    t.state
    <- Grammared.apply_action t.state (Edit_starting_at_nth_line first_line_changed_index)
  ;;

  let print_status t =
    Grammared.errors_unsorted t.state
    |> List.sort ~compare:(fun a b ->
      Int.compare a.position.offset_start b.position.offset_start)
    |> List.map
         ~f:
           (Syntax_error.to_string_referencing_input
              ~input_string:(String.concat_lines (Doc.to_lines t.doc)))
    |> List.iter ~f:print_endline;
    if Grammared.is_done t.state then print_endline "Done!"
  ;;
end

let%expect_test "basic parse through." =
  let s =
    {|(1 4 2 6
3 6
3.2  8x
7 3
4 6
2
(abcd 1 4 6) 3.14)|}
  in
  let handle = Handle.create s in
  (* No errors at first. *)
  Handle.print_status handle;
  [%expect {| |}];
  (* No errors in first 2 lines. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect {| |}];
  (* We get an error! *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.2`.
    Maybe you meant `3`?

    3| 3.2  8x
       ^^^
    Expected an integer value, like -3 or 10.
    Parsed: `8x`.

    3| 3.2  8x
            ^^
    |}];
  (* Next chunk has no errors, but we still get errors from before. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.2`.
    Maybe you meant `3`?

    3| 3.2  8x
       ^^^
    Expected an integer value, like -3 or 10.
    Parsed: `8x`.

    3| 3.2  8x
            ^^
    |}];
  (* And now we get more errors, but the ones that came before are preserved. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.2`.
    Maybe you meant `3`?

    3| 3.2  8x
       ^^^
    Expected an integer value, like -3 or 10.
    Parsed: `8x`.

    3| 3.2  8x
            ^^
    Expected an int.
    Parsed: `(abcd 1 4 6)`.

    7| (abcd 1 4 6) 3.14)
       ^^^^^^^^^^^^
    Integer values cannot be floats.
    Parsed: `3.14`.
    Maybe you meant `3`?

    7| (abcd 1 4 6) 3.14)
                    ^^^^
    Done!
    |}];
  (* Let's replace everything after the first error.
     We should still get those errors. *)
  Handle.update_doc
    handle
    {|(1 4 2 6
3 6
3.2  8x
7 3
1.2)|};
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.2`.
    Maybe you meant `3`?

    3| 3.2  8x
       ^^^
    Expected an integer value, like -3 or 10.
    Parsed: `8x`.

    3| 3.2  8x
            ^^
    |}];
  (* And with parsing, we should be done. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `3.2`.
    Maybe you meant `3`?

    3| 3.2  8x
       ^^^
    Expected an integer value, like -3 or 10.
    Parsed: `8x`.

    3| 3.2  8x
            ^^
    Integer values cannot be floats.
    Parsed: `1.2`.
    Maybe you meant `1`?

    5| 1.2)
       ^^^
    Done!
    |}];
  (* Finally, we update, which should introduce an error.
     We won't get it now, but we should later! *)
  Handle.update_doc
    handle
    {|(1 4 2 6
1.5)|};
  Handle.print_status handle;
  [%expect {| |}];
  (* And now that error should show up. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `1.5`.
    Maybe you meant `1`?

    2| 1.5)
       ^^^
    Done!
    |}];
  (* And one chunk to assert that we're done. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `1.5`.
    Maybe you meant `1`?

    2| 1.5)
       ^^^
    Done!
    |}];
  (* Let's assert that attempting to parse after we're done shouldn't do anything. *)
  Handle.parse_chunk handle;
  Handle.print_status handle;
  [%expect
    {|
    Integer values cannot be floats.
    Parsed: `1.5`.
    Maybe you meant `1`?

    2| 1.5)
       ^^^
    Done!
    |}]
;;
