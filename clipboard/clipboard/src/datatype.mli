open Core

(** Type representing the possible data types of a clipboard content.

    https://www.w3.org/TR/clipboard-apis/#mandatory-data-types-x *)
type t =
  | Text_plain
  | Text_uri_list
  | Text_csv
  | Text_html
  | Application_json
  | Other of string
[@@deriving compare, sexp]

include Comparable.S with type t := t
include Stringable.S with type t := t
