(** Type that can be used to turn a clipboard content of specific Datatype into a list of
    rows, each row as a list of strings. See Incr_dom_list.Clipboard for an example usage. *)
type t

val create : Datatype.t -> (string -> string list list) -> t
val data_type : t -> Datatype.t
val rows_of_input : t -> string -> string list list
