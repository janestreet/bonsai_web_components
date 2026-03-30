(** Type that can be used to turn a list of rows, each row as a list of strings, into a
    clipboard content of specific Datatype. See Incr_dom_list.Clipboard for an example
    usage. *)
type t

val create : Datatype.t -> (?headers:string list -> string list list -> string) -> t
val data_type : t -> Datatype.t
val rows_to_output : t -> ?headers:string list -> string list list -> string
