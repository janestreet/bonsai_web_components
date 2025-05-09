(** When intercepting the paste copy event, Parsers help turn the raw pasted text into a
    list of rows. *)
module type S = sig
  (** Data type that this parser can parse. *)

  (** Returns the parsed rows. *)
  val rows_of_input : string -> string list list
end
