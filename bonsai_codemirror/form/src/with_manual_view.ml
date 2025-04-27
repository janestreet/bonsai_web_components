open! Core
open Bonsai.Let_syntax
module Codemirror_ui = Bonsai_web_ui_codemirror
module Form = Bonsai_web_ui_form.With_manual_view

module Make_forms (M : sig
    type t

    val create_codemirror
      :  name:string
      -> t
      -> local_ Bonsai.graph
      -> Codemirror_ui.t Bonsai.t
  end) =
struct
  let string ?(name = "Bonsai_web_ui_codemirror_form") t (local_ graph) =
    let codemirror = M.create_codemirror ~name t graph in
    let%arr codemirror in
    { Form.view = Codemirror_ui.view codemirror
    ; value = Ok (Codemirror_ui.text codemirror)
    ; set =
        (fun s ->
          Codemirror_ui.send_transaction
            codemirror
            (Codemirror_ui.Transaction.set_lines (String.split_lines s)))
    }
  ;;

  let stringable (type a) (module M : Stringable with type t = a) ?name t (local_ graph) =
    let%map.Bonsai form = string ?name t graph in
    Form.project form ~parse_exn:M.of_string ~unparse:M.to_string
  ;;

  let sexpable (type a) (module M : Sexpable with type t = a) ?name t (local_ graph) =
    let%map.Bonsai form = string ?name t graph in
    Form.project
      form
      ~parse_exn:(fun s -> [%of_sexp: M.t] (Sexp.of_string s))
      ~unparse:(fun t -> Sexp.to_string_hum ~indent:2 ([%sexp_of: M.t] t))
  ;;
end

module Basic = struct
  module Forms = Make_forms (struct
      type t = Codemirror.State.Extension.t list

      let create_codemirror ~name extensions =
        Codemirror_ui.of_initial_state ~name (Codemirror_initial_state.create extensions)
      ;;
    end)

  let string ?name ?(extensions = []) () = Forms.string ?name extensions

  let stringable (type a) ?name ?(extensions = []) (module M : Stringable with type t = a)
    =
    Forms.stringable (module M) ?name extensions
  ;;

  let sexpable (type a) ?name ?(extensions = []) (module M : Sexpable with type t = a) =
    Forms.sexpable (module M) ?name extensions
  ;;
end

module Dynamic_extensions = struct
  type t =
    | T :
        { sexp_of_model : ('a -> Sexp.t) option
        ; equal : 'a -> 'a -> bool
        ; value : 'a Bonsai.t
        ; compute_extensions : ('a -> Codemirror.State.Extension.t list) Bonsai.t
        }
        -> t

  module Forms = Make_forms (struct
      type nonrec t = t

      let create_codemirror ~name (T { sexp_of_model; value; compute_extensions; equal }) =
        Codemirror_ui.with_dynamic_extensions
          ?sexp_of:sexp_of_model
          ~equal
          ~name
          ~initial_state:Codemirror_initial_state.empty
          ~compute_extensions
          value
      ;;
    end)

  let string ?sexp_of_model ?name ~equal ~compute_extensions value =
    Forms.string ?name (T { sexp_of_model; equal; value; compute_extensions })
  ;;

  let stringable
    (type a)
    (module S : Stringable with type t = a)
    ?sexp_of_model
    ?name
    ~equal
    ~compute_extensions
    value
    =
    Forms.stringable
      (module S)
      ?name
      (T { sexp_of_model; equal; value; compute_extensions })
  ;;

  let sexpable
    (type a)
    (module S : Sexpable with type t = a)
    ?sexp_of_model
    ?name
    ~equal
    ~compute_extensions
    value
    =
    Forms.sexpable
      (module S)
      ?name
      (T { sexp_of_model; equal; value; compute_extensions })
  ;;
end
