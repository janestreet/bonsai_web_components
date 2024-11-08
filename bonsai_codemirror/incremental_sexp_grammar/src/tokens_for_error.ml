open! Core
module Token = Tokenizer.Token

let max_length = 12

type t =
  { tokens_rev : Token.t Nonempty_list.t
  ; last : Token.t option
  ; truncated : bool
  }
[@@deriving sexp_of, compare]

let singleton token =
  { tokens_rev = Nonempty_list.singleton token; last = None; truncated = false }
;;

let tokens_maybe_trunc t =
  match t.last with
  | Some token -> Nonempty_list.cons token t.tokens_rev |> Nonempty_list.reverse
  | None -> t.tokens_rev |> Nonempty_list.reverse
;;

let push t token =
  match t.last with
  | Some _ -> { t with last = Some token; truncated = true }
  | None when Nonempty_list.length t.tokens_rev = max_length - 1 ->
    { t with last = Some token; truncated = false }
  | None -> { t with tokens_rev = Nonempty_list.cons token t.tokens_rev }
;;

let create hd tl = List.fold tl ~init:(singleton hd) ~f:(fun acc token -> push acc token)

let to_string t =
  let ending =
    match t.last with
    | None -> ""
    | Some token ->
      let trunc_message = if t.truncated then "<truncated>" else "" in
      trunc_message ^ Token.to_sexp_string [ token ]
  in
  let tokens = Nonempty_list.reverse t.tokens_rev in
  Token.to_sexp_string (Nonempty_list.to_list tokens) ^ ending
;;

let first_token { tokens_rev; _ } = Nonempty_list.last tokens_rev
let offset_start t = (first_token t).pos.offset_start

let offset_end { tokens_rev; last; _ } =
  let last = Option.value last ~default:(Nonempty_list.hd tokens_rev) in
  last.pos.offset_end
;;
