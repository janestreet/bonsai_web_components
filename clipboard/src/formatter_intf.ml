(** When intercepting the copy event, Formatters help outsource turning text from table
    cells rows into a format readable by external applications. *)
module type S = sig
  (** Output returned by this formatter is of this datatype. *)
  val data_type : Datatype.t
end
