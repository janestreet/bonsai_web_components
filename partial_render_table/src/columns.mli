open! Core
open! Bonsai_web
module Sort_kind := Bonsai_web_ui_partial_render_table_protocol.Sort_kind

module Column_structure : sig
  type 'column_id t

  val flat : 'column_id list -> 'column_id t
  val flat_dynamic : 'column_id list Bonsai.t -> 'column_id t

  type 'column_id structure := 'column_id t

  module Group : sig
    type 'column_id t

    val leaf : 'column_id -> 'column_id t
    val group : label:Vdom.Node.t Bonsai.t -> 'column_id t list -> 'column_id t
    val lift : 'column_id t list -> 'column_id structure
  end

  module Group_dynamic : sig
    type 'column_id t

    val leaf : 'column_id -> 'column_id t
    val group : label:Vdom.Node.t -> 'column_id t list -> 'column_id t
    val lift : 'column_id t list Bonsai.t -> 'column_id structure
  end

  val with_initial_widths
    :  'column_id t
    -> f:('column_id -> Css_gen.Length.t) Bonsai.t
    -> 'column_id t

  val with_is_resizable : 'column_id t -> f:('column_id -> bool) Bonsai.t -> 'column_id t
  val default_initial_width : Css_gen.Length.t
end

module Render_cell : sig
  type ('key, 'data, 'column_id) t =
    | Pure of ('column_id -> 'key -> 'data -> Vdom.Node.t) Bonsai.t
    | Stateful_rows of
        ('key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> ('column_id -> Vdom.Node.t) Bonsai.t)
    | Stateful_cells of
        ('column_id Bonsai.t
         -> 'key Bonsai.t
         -> 'data Bonsai.t
         -> local_ Bonsai.graph
         -> Vdom.Node.t Bonsai.t)
end

module Basic : sig
  val build
    :  ?sorts:
         ('column_id Bonsai.t
          -> local_ Bonsai.graph
          -> ('key, 'data) Sort_kind.t option Bonsai.t)
    -> ('column_id, _) Comparator.Module.t
    -> columns:'column_id Column_structure.t
    -> render_header:('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> render_cell:('key, 'data, 'column_id) Render_cell.t
    -> ('key, 'data, 'column_id) Column_intf.with_sorter

  module Sortable = Sortable
end

module Expert : sig
  val build
    :  ('column_id, _) Comparator.Module.t
    -> columns:'column_id Column_structure.t
    -> render_header:('column_id Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
    -> render_cell:('key, 'data, 'column_id) Render_cell.t
    -> ('key, 'data, 'column_id) Column_intf.t

  module Sortable = Sortable
end
