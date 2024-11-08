open Core

module Kind = struct
  type t =
    | Incorrect_capitalization
    | Possible_spelling_mistake
  [@@deriving sexp_of, compare]
end

type t =
  { kind : Kind.t option
  ; correction : string
  }
[@@deriving sexp_of, compare]

let capitalization s = { kind = Some Incorrect_capitalization; correction = s }
let spelling_mistake s = { kind = Some Possible_spelling_mistake; correction = s }
let other s = { kind = None; correction = s }

let spellcheck ~possible_corrections value =
  let lowercase_value = String.lowercase value in
  match
    List.find possible_corrections ~f:(fun correction ->
      String.equal lowercase_value (String.lowercase correction))
  with
  | Some correction -> [ { kind = Some Incorrect_capitalization; correction } ]
  | None ->
    String_distance.Levenshtein.n_closest
      ~n:(List.length possible_corrections)
      ~cutoff:2
      lowercase_value
      possible_corrections
      ~f:String.lowercase
    |> List.map ~f:(fun (correction, _) ->
      { kind = Some Possible_spelling_mistake; correction })
;;

let list_to_string ls =
  let kinds =
    List.map ls ~f:(fun { kind; _ } -> kind)
    |> List.partition3_map ~f:(function
      | None -> `Fst ()
      | Some Incorrect_capitalization -> `Snd ()
      | Some Possible_spelling_mistake -> `Trd ())
  in
  let label =
    match kinds with
    | [], [], [] -> None
    | _, [], [] -> None
    | [], _, [] -> Some "Incorrect capitalization"
    | [], [], _ -> Some "Possible spelling mistake"
    | _, _, _ -> Some "Possible incorrect capitalization or spelling"
  in
  let suggestions =
    match List.rev ls with
    | [] -> None
    | [ { correction; _ } ] -> Some [%string "`%{correction}`"]
    | [ { correction = c2; _ }; { correction = c1; _ } ] ->
      Some [%string "`%{c1}` or `%{c2}`"]
    | { correction = last; _ } :: rest ->
      let rest_str =
        List.map rest ~f:(fun { correction; _ } -> [%string "`%{correction}`"])
        |> List.rev
        |> String.concat ~sep:", "
      in
      Some [%string "%{rest_str} or `%{last}`"]
  in
  let%map.Option suggestions in
  match label with
  | None -> [%string "Maybe you meant %{suggestions}?"]
  | Some label -> [%string "%{label}; maybe you meant %{suggestions}?"]
;;
