(** Outputs data in HTML form readable by many applications such as Gmail, Excel.

    Datatype used is "text/html".

    Note that when the clipboard content contains both "text/plain" and "text/html"
    datatypes, applications tend to prefer parsing "text/html". *)
module Formatter : sig
  include Formatter_intf.S

  (** Create the formatter.

      [border] adds 1x solid black border to the table. Default is false. Pasting content
      containing border to Excel preserves the border by default, because Excel uses
      "text/html" type. This can be circumvented by telling Excel to paste only values. *)
  val create : ?border:bool -> unit -> Formatter.t
end
