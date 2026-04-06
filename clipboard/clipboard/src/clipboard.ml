(** This library contains helper functions for interacting with the system clipboard.
    Essentially forms a basis for the `js_clipboard` library, while not depending on
    Js_of_ocaml. The reason for separation is testability of functions in this library.

    One important concept to understand is that a clipboard can contain multiple different
    representation keyed by datatype. And that as an application one can decide which
    datatype to parse upon a paste event. Many applications seem to prefer text/html over
    text/plain. Surprisingly even excel which expects text/plain to contain tab separated
    csv, will if presented with clipboard containing both text/plain and text/html will
    prefer to parse a html table from the text/html. *)
module Datatype = Datatype

module Formatter = Formatter
module Parser = Parser
module Excel = Excel
module Html = Html
