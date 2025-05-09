val formatter : Formatter.t
val parser : Parser.t

(** Outputs data in a form readable by (i.e. pastable to) Excel and similar spreadsheet
    applications.

    Datatype is "text/plain".

    Note that when the clipboard content contains both "text/plain" and "text/html"
    datatypes, applications tend to prefer parsing "text/html". *)
module Formatter : sig
  include Formatter_intf.S

  val create : unit -> Formatter.t
end

(** Parses data generated and copied from Excel and similar spreadsheet applications.

    Datatype is "text/plain". *)
module Parser : sig
  include Parser_intf.S

  val create : unit -> Parser.t
end
