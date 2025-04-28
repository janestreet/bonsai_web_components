open! Core
open! Bonsai.Let_syntax
open! Import
open! Bonsai_web_test
module Auto_generated = Bonsai_web_ui_auto_generated

module M = struct
  type t = Foo [@@deriving sexp]

  let t_sexp_grammar =
    { Sexp_grammar.untyped =
        Lazy
          (Portable_lazy.from_fun (fun () : Sexp_grammar.grammar ->
             Union
               [ Variant
                   { case_sensitivity = Case_insensitive
                   ; clauses = [ No_tag { name = "foo"; clause_kind = Atom_clause } ]
                   }
               ; Variant
                   { case_sensitivity = Case_insensitive
                   ; clauses =
                       [ No_tag
                           { name = "foo"
                           ; clause_kind = List_clause { args = Cons (List Empty, Empty) }
                           }
                       ]
                   }
               ]))
    }
  ;;
end

let%expect_test "Does not crash." =
  let handle =
    Handle.create (form_result_spec [%sexp_of: M.t]) (Auto_generated.form (module M))
  in
  Handle.show handle;
  Handle.do_actions handle [ Foo ];
  [%expect
    {|
    (Error (
      Failure
      "Sexplib.Sexp.of_string: incomplete S-expression while in state Parsing_toplevel_whitespace: "))

    ==============
    <textarea placeholder="" id="bonsai_path_replaced_in_test" value:normalized="" @on_input> </textarea>
    |}];
  ()
;;
