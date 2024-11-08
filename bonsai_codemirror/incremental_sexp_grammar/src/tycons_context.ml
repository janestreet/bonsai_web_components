open! Core

module Tycon = struct
  module T = struct
    type t =
      { name : string
      ; args : Sexp_grammar.grammar list
      ; defns : Sexp_grammar.defn list
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Context_frame = struct
  type t =
    { resolved_tyvars : Sexp_grammar.grammar Or_error.t Lazy.t String.Map.t
    ; tycon : Tycon.t
    }
  [@@deriving sexp_of]

  let rec map_tag_list tag_list ~f =
    match tag_list with
    | Sexp_grammar.No_tag grammar ->
      let%map.Or_error grammar = f grammar in
      Sexp_grammar.No_tag grammar
    | Tag { key; value; grammar } ->
      let%map.Or_error grammar = map_tag_list grammar ~f in
      Sexp_grammar.Tag { key; value; grammar }
  ;;

  let rec resolve_tyvars previous_frame grammar =
    let loop = resolve_tyvars previous_frame in
    match grammar with
    | Sexp_grammar.Tyvar name ->
      (match previous_frame with
       | None ->
         Or_error.error_s
           [%message "cannot use [Tyvar] unless within the scope of a [Tycon]"]
       | Some { resolved_tyvars; _ } ->
         (match Map.find resolved_tyvars name with
          | None -> Or_error.error_s [%message "unbound [Tyvar]" (name : string)]
          | Some value -> force value))
    | (Any _ | String | Bool | Char | Float | Integer) as g -> Ok g
    | Lazy g ->
      let%map.Or_error g = loop (force g) in
      Sexp_grammar.Lazy (lazy g)
    | Option g ->
      let%map.Or_error g = loop g in
      Sexp_grammar.Option g
    | Union gs ->
      let%map.Or_error gs = List.map gs ~f:loop |> Or_error.all in
      Sexp_grammar.Union gs
    | Variant { clauses; case_sensitivity } ->
      let%map.Or_error clauses =
        List.map clauses ~f:(fun clause ->
          map_tag_list clause ~f:(function
            | { name = _; clause_kind = Atom_clause } as atom -> Ok atom
            | { name; clause_kind = List_clause { args } } ->
              let%map.Or_error args = resolve_list_tyvars previous_frame args in
              { Sexp_grammar.name; clause_kind = List_clause { args } }))
        |> Or_error.all
      in
      Sexp_grammar.Variant { clauses; case_sensitivity }
    | List list_grammar ->
      let%map.Or_error list_grammar = resolve_list_tyvars previous_frame list_grammar in
      Sexp_grammar.List list_grammar
    | Tagged { key; value; grammar } ->
      let%map.Or_error grammar = loop grammar in
      Sexp_grammar.Tagged { key; value; grammar }
    | Recursive (name, params) ->
      (match previous_frame with
       | None ->
         Or_error.error_s [%message "cannot use [Recursive] unless within a [Tycon]"]
       | Some previous_frame -> loop (Tycon (name, params, previous_frame.tycon.defns)))
    | Tycon (name, params, defns) ->
      let%map.Or_error params = List.map params ~f:loop |> Or_error.all in
      Sexp_grammar.Tycon (name, params, defns)

  and resolve_list_tyvars previous_frame grammar =
    match grammar with
    | Empty -> Ok Sexp_grammar.Empty
    | Many grammar ->
      let%map.Or_error grammar = resolve_tyvars previous_frame grammar in
      Sexp_grammar.Many grammar
    | Cons (first, rest) ->
      let%bind.Or_error first = resolve_tyvars previous_frame first in
      let%map.Or_error rest = resolve_list_tyvars previous_frame rest in
      Sexp_grammar.Cons (first, rest)
    | Fields { allow_extra_fields; fields } ->
      let%map.Or_error fields =
        List.map fields ~f:(fun field ->
          map_tag_list field ~f:(fun { name; required; args } ->
            let%map.Or_error args = resolve_list_tyvars previous_frame args in
            { Sexp_grammar.name; required; args }))
        |> Or_error.all
      in
      Sexp_grammar.Fields { allow_extra_fields; fields }
  ;;

  let create ~previous_frame (tycon : Tycon.t) =
    match List.find tycon.defns ~f:(fun defn -> String.equal defn.tycon tycon.name) with
    | None ->
      Or_error.error_string
        [%string
          "Invalid grammar. Recursive type/constructor name not found: %{tycon.name}"]
    | Some defn ->
      (match List.zip defn.tyvars tycon.args with
       | Unequal_lengths ->
         Or_error.error_s
           [%message
             "wrong number of type variable parameters"
               (defn.tyvars : string list)
               (tycon.args : Sexp_grammar.grammar list)]
       | Ok tyvars ->
         let resolved =
           List.Assoc.map tyvars ~f:(fun grammar ->
             lazy (resolve_tyvars previous_frame grammar))
         in
         (match String.Map.of_alist resolved with
          | `Duplicate_key name ->
            Or_error.error_s [%message "duplicate type variable name" (name : string)]
          | `Ok resolved_tyvars -> Ok (defn.grammar, { resolved_tyvars; tycon })))
  ;;
end

type t = Context_frame.t list [@@deriving sexp_of]

let empty = []

let resolve (t : t) name =
  match t with
  | { resolved_tyvars; _ } :: _ ->
    (match Map.find resolved_tyvars name with
     | Some grammar -> force grammar
     | None -> Or_error.error_s [%message "unbound [Tyvar]" (name : string)])
  | [] -> Or_error.error_s [%message "No tyvars registered"]
;;

let register (t : t) tycon =
  let previous_frame =
    match t with
    | top :: _ -> Some top
    | [] -> None
  in
  let%map.Or_error defn_grammar, new_frame = Context_frame.create ~previous_frame tycon in
  defn_grammar, new_frame :: t
;;

let pop = function
  | _ :: tl -> Ok tl
  | [] -> Or_error.error_s [%message "No tyvars registered"]
;;

let curr_defns = function
  | { Context_frame.tycon = { defns; _ }; _ } :: _ -> Some defns
  | [] -> None
;;

module For_testing = struct
  let largest_frame_num_tyvars (t : t) =
    let max = ref 0 in
    List.iter t ~f:(fun { resolved_tyvars; _ } ->
      max := Int.max !max (Map.length resolved_tyvars));
    !max
  ;;

  let num_frames = List.length
end
