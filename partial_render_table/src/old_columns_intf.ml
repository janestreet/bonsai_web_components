open! Core
open! Bonsai_web
open Bonsai_web_ui_partial_render_table_protocol

module type Basic = sig
  (** There are a few ways to specify the columns on a partial render, table, and they
      each have their own tradeoffs and capibilities. You can not mix-and-match column
      kinds. Read the doc comments for each of the submodules to learn more. *)
  module Indexed_column_id : sig
    type t [@@deriving equal, sexp]

    val of_int : int -> t
    val to_int : t -> int
  end

  type ('key, 'data, 'column_id) t
  type ('key, 'data, 'column_id) columns := ('key, 'data, 'column_id) t

  module Dynamic_experimental : sig
    module Sort_kind : sig
      type ('key, 'data) sort := 'key * 'data -> 'key * 'data -> int

      (** A [Sort_kind.t] consists of [forward] (ascending) and [reverse] (descending)
          sorting implementations. *)
      type ('key, 'data) t = ('key, 'data) Sort_kind.t =
        { forward : ('key, 'data) sort
        ; reverse : ('key, 'data) sort
        }

      (** Most sort functions are reversible, so you can create a [t] from a "forward"
          sort function via [reversible], or a "backward" sort function via [reversible']. *)
      val reversible : forward:('key, 'data) sort -> ('key, 'data) t

      val reversible' : reverse:('key, 'data) sort -> ('key, 'data) t
    end

    val build
      :  ?sorts:
           ('column_id Bonsai.t
            -> local_ Bonsai.graph
            -> ('key, 'data) Sort_kind.t option Bonsai.t)
      -> ('column_id, _) Comparator.Module.t
      -> columns:'column_id list Bonsai.t
      -> render_header:
           ('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:
           ('column_id Bonsai.t
            -> 'key Bonsai.t
            -> 'data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
      -> ('key, 'data, 'column_id) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Old_columns.Dynamic_experimental.Sortable
  end

  module Dynamic_cells : sig
    (** Dynamic_cells is a column-specification format with the following tradeoffs:

        - Pro: Each cell is it's own bonsai computation, so you can stick complex
          components in side of them, like forms, or graphs.
        - Con: The set of columns must be statically known ahead of time, and can not be
          determined dynamically. *)

    type ('key, 'data) t

    val column
      :  ?sort:('key * 'data -> 'key * 'data -> int) Bonsai.t
           (** If this column is sortable, you can provide the sorting function here *)
      -> ?sort_reversed:('key * 'data -> 'key * 'data -> int) Bonsai.t
           (** If the column has a specialized "reverse order", you can provide it here. *)
      -> ?initial_width:Css_gen.Length.t
      -> ?visible:bool Bonsai.t
           (** [visible] can be set to [false] to hide the whole column. *)
      -> ?resizable:bool Bonsai.t
           (** [resizable] can be set to [false] disable resizing for this column. *)
      -> header:Vdom.Node.t Bonsai.t
           (** [header] determines the contents of the column header. *)
      -> cell:
           (key:'key Bonsai.t
            -> data:'data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
           (** [cell] is the function determines the contents of every cell in this
               column. *)
      -> unit
      -> ('key, 'data) t

    (** [group ~label children] builds a header-group that has [children] underneath it.
        The content of header-group is set to [label] *)
    val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t

    (** [expand ~label child] builds a header-group that has a single child underneath it. *)
    val expand : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t -> ('key, 'data) t

    (** [lift] pulls a list of columns out into a column specification for use in the
        primary APIs *)
    val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Old_columns.Dynamic_cells_with_sorter.Sortable
  end

  module Dynamic_columns : sig
    (** Dynamic_columns is a column-specification format with the following tradeoffs:

        - Pro: The set of columns, and how to render them can be determined dynamically
          ([lift] takes a column list inside a Value.t)
        - Con: Cells are computed with plain functions, and can not maintain state. *)
    type ('key, 'data) t

    val column
      :  ?sort:('key * 'data -> 'key * 'data -> int)
           (** If this column is sortable, you can provide the sorting function here *)
      -> ?sort_reversed:('key * 'data -> 'key * 'data -> int)
           (** If the column has a specialized "reverse order", you can provide it here. *)
      -> ?initial_width:Css_gen.Length.t
      -> ?visible:bool (** [visible] can be set to [false] to hide the whole column. *)
      -> ?resizable:bool
           (** [resizable] can be set to [false] disable resizing for this column. *)
      -> header:Vdom.Node.t (** [header] determines the contents of the column header. *)
      -> cell:(key:'key -> data:'data -> Vdom.Node.t)
           (** [cell] is the function determines the contents of every cell in this
               column. *)
      -> unit
      -> ('key, 'data) t

    (** [group ~label children] builds a header-group that has [children] underneath it.
        The content of header-group is set to [label] *)
    val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t

    (** [lift] pulls a list of columns out into a column specification for use in the
        primary APIs *)
    val lift : ('key, 'data) t list Bonsai.t -> ('key, 'data, Indexed_column_id.t) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Old_columns.Dynamic_columns_with_sorter.Sortable
  end
end

module type Expert = sig
  module Indexed_column_id : sig
    type t [@@deriving equal, sexp]

    val of_int : int -> t
    val to_int : t -> int
  end

  type ('key, 'data, 'column_id) t
  type ('key, 'data, 'column_id) columns := ('key, 'data, 'column_id) t

  module Dynamic_experimental : sig
    val build
      :  ('column_id, 'a) Comparator.Module.t
      -> columns:'column_id list Bonsai.t
      -> render_header:
           ('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
      -> render_cell:
           ('column_id Bonsai.t
            -> 'key Bonsai.t
            -> 'data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
      -> ('key, 'data, 'column_id) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Sortable
  end

  module Dynamic_cells : sig
    type ('key, 'data) t

    val column
      :  ?initial_width:Css_gen.Length.t
      -> ?visible:bool Bonsai.t
      -> ?resizable:bool Bonsai.t
      -> header:Vdom.Node.t Bonsai.t
      -> cell:
           (key:'key Bonsai.t
            -> data:'data Bonsai.t
            -> local_ Bonsai.graph
            -> Vdom.Node.t Bonsai.t)
      -> unit
      -> ('key, 'data) t

    val group : label:Vdom.Node.t Bonsai.t -> ('key, 'data) t list -> ('key, 'data) t
    val lift : ('key, 'data) t list -> ('key, 'data, Indexed_column_id.t) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Sortable
  end

  module Dynamic_columns : sig
    type ('key, 'data) t

    val column
      :  ?initial_width:Css_gen.Length.t
      -> ?visible:bool
      -> ?resizable:bool
      -> header:Vdom.Node.t
      -> cell:(key:'key -> data:'data -> Vdom.Node.t)
      -> unit
      -> ('key, 'data) t

    val group : label:Vdom.Node.t -> ('key, 'data) t list -> ('key, 'data) t
    val lift : ('key, 'data) t list Bonsai.t -> ('key, 'data, Indexed_column_id.t) columns

    (** [Sortable] provides types, state, and ui helper functions to sort your table data
        by one or more columns. *)
    module Sortable = Sortable
  end
end
