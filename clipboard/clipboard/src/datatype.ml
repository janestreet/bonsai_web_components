open Core

module T = struct
  type t =
    | Text_plain
    | Text_uri_list
    | Text_csv
    | Text_html
    | Application_json
    | Other of string
  [@@deriving compare, sexp]
end

include T

let to_string = function
  | Text_plain -> "text/plain"
  | Text_uri_list -> "text/uri-list"
  | Text_csv -> "text/csv"
  | Text_html -> "text/html"
  | Application_json -> "application/json"
  | Other str -> str
;;

let of_string = function
  | "text/plain" -> Text_plain
  | "text/uri-list" -> Text_uri_list
  | "text/csv" -> Text_csv
  | "text/html" -> Text_html
  | "application/json" -> Application_json
  | s -> Other s
;;

include Comparable.Make (T)
