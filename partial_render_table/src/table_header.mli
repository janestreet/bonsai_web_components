open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Js_of_ocaml
open! Incr_map_collate

val component
  :  column_id_equal:('column_id -> 'column_id -> bool)
  -> themed_attrs:Table_view.Themed.t Bonsai.t
  -> resize_column_widths_to_fit:bool Bonsai.t
  -> focused_column:'column_id option Bonsai.t
  -> 'column_id Header_tree.t Bonsai.t
  -> column_widths:('column_id, Column_size.t, 'column_id_cmp) Map.t Bonsai.t
  -> set_column_width:
       (column_id:'column_id -> [ `Px_float of float ] -> unit Vdom.Effect.t) Bonsai.t
  -> set_column_width_for_reporting:
       (column_id:'column_id -> [ `Px_float of float ] -> unit Vdom.Effect.t) Bonsai.t
  -> set_header_client_rect:
       (Bonsai_web_ui_element_size_hooks.Visibility_tracker.Bbox.t -> unit Vdom.Effect.t)
         Bonsai.t
  -> local_ Bonsai.graph
  -> Table_view.Header.t Bonsai.t
