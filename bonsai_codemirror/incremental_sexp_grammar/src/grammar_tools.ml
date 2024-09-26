open! Core

let preview_up_to = 3
let preview_max_depth = 3

let rec strip_tags = function
  | Sexp_grammar.Tag { grammar; _ } -> strip_tags grammar
  | No_tag res -> res
;;

let record_arg_grammar = function
  | Sexp_grammar.Empty -> Ok None
  | Cons (grammar, Empty) -> Ok (Some grammar)
  | invalid_grammar ->
    Or_error.error_s
      [%message
        "Invalid sexp grammar: record fields must take exactly zero or one args."
          (invalid_grammar : Sexp_grammar.list_grammar)]
;;

let rec to_string ?(depth = 0) =
  let invalid = " <INVALID_SEXP_GRAMMAR>" in
  let recurse g =
    if depth >= preview_max_depth then "..." else to_string ~depth:(depth + 1) g
  in
  let concat_trunc ~sep ~to_string all =
    let main =
      List.take all preview_up_to |> List.map ~f:to_string |> String.concat ~sep
    in
    let trunc = if List.length all > preview_up_to then "..." else "" in
    main ^ trunc
  in
  function
  | Sexp_grammar.Any _ -> "<any>"
  | Bool -> "<bool>"
  | Char -> "<char>"
  | Float -> "<float>"
  | Integer -> "<int>"
  | String -> "<string>"
  | Option g -> [%string "<option%{recurse g}>"]
  | Lazy g -> recurse (force g)
  | Tagged { grammar; _ } -> recurse grammar
  | Union grammars ->
    let inner = concat_trunc ~sep:"|" ~to_string:recurse grammars in
    [%string "<union%{inner}>"]
  | Variant { clauses; _ } ->
    let to_string clause_tagged =
      let { Sexp_grammar.name; clause_kind } = strip_tags clause_tagged in
      let contents =
        match clause_kind with
        | Atom_clause -> ""
        | List_clause { args = Cons (grammar, Empty) } -> " " ^ recurse grammar
        | List_clause _ -> invalid
      in
      [%string "(%{name}%{contents})"]
    in
    let inner = concat_trunc ~sep:"|" ~to_string clauses in
    [%string "<variant%{inner}>"]
  | List Empty -> "<empty_list>"
  | List (Many grammar) -> [%string "<list%{recurse grammar}>"]
  | List (Cons (grammar, Empty)) -> [%string "[%{recurse grammar}]"]
  | List (Cons (grammar, tl)) -> [%string "[%{recurse grammar}::%{recurse (List tl)}]"]
  | List (Fields { fields; allow_extra_fields }) ->
    let all_fields =
      List.map fields ~f:strip_tags
      |> List.map ~f:(fun { name; required; args } ->
        let arg_name =
          match record_arg_grammar args with
          | Ok None -> ""
          | Ok (Some grammar) -> " " ^ recurse grammar
          | Error _ -> invalid
        in
        let required_flag = if required then "" else "?" in
        [%string "(%{name}%{required_flag}%{arg_name})"])
    in
    let fields_to_display = List.take all_fields preview_up_to in
    let show_more =
      if List.length all_fields > preview_up_to || allow_extra_fields then "..." else ""
    in
    "(" ^ String.concat (fields_to_display @ [ show_more ]) ^ ")"
  | Tycon _ | Tyvar _ | Recursive _ -> "<recursive>"
;;

let arg_to_string = function
  | Sexp_grammar.Cons (grammar, Empty) -> to_string grammar
  | grammar -> to_string (List grammar)
;;
