open! Core

type t = Sexp_grammar.grammar Nonempty_list.t String.Map.t [@@deriving sexp_of]

let empty = String.Map.empty

let resolve (t : t) name =
  match Map.find t name with
  | Some (x :: _) -> Some x
  | None -> None
;;

let register (t : t) name grammar =
  let%map.Option grammar_to_register =
    match grammar with
    | Sexp_grammar.Tyvar name' -> resolve t name'
    | _ -> Some grammar
  in
  Map.update t name ~f:(function
    | None -> [ grammar_to_register ]
    | Some gs -> Nonempty_list.cons grammar_to_register gs)
;;

let register_many (t : t) names grammars =
  match List.zip names grammars with
  | Ok zipped ->
    List.fold_left zipped ~init:(Ok t) ~f:(fun acc (name, grammar) ->
      let%bind.Or_error tyvars = acc in
      register tyvars name grammar
      |> Or_error.of_option
           ~error:
             (Error.of_lazy_sexp
                (lazy
                  [%message
                    "Internal error: self-referencing tyvar without a definition"
                      (name : string)
                      (tyvars : t)])))
  | Unequal_lengths ->
    Or_error.error_s
      [%message
        "Internal error: unequal length lists for defn's list of tyvars and the tyvars \
         defined in the tycon"]
;;

let unregister (t : t) name =
  if not (Map.mem t name)
  then None
  else
    Map.change t name ~f:(function
      | None | Some [ _ ] -> None
      | Some (_ :: tl_hd :: tl) -> Some (tl_hd :: tl))
    |> Some
;;

let unregister_many t names =
  List.fold names ~init:(Ok t) ~f:(fun acc to_remove ->
    let%bind.Or_error tyvars = acc in
    unregister tyvars to_remove
    |> Or_error.of_option
         ~error:
           (Error.of_lazy_sexp
              (lazy
                [%message
                  "Internal error: couldn't unregister tyvar"
                    (to_remove : string)
                    (tyvars : t)])))
;;

module For_testing = struct
  let max_size (t : t) =
    let max = ref 0 in
    Map.iter t ~f:(fun data -> max := Int.max !max (Nonempty_list.length data));
    !max
  ;;
end
