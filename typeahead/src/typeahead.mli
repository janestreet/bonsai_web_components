open! Core
open! Bonsai_web

(** These controls come unstyled by default. jane-web-style provides css that will make
    the control and option pills pretty. *)

(** A ['a t] represents a typeahead whose value has type ['a].

    [current_input] gives access to the current contents of the form's [<input>] element *)
type 'a t =
  { selected : 'a
  ; set_selected : 'a -> unit Ui_effect.t
  ; current_input : string
  ; view : Vdom.Node.t
  }

module Attr_merge_behavior : sig
  (** This module defines how the attrs in the typeahead should be merged
      [Legacy_do_not_merge] is the original behavior, and calls
      [Vdom.Attr.many_without_merge] [Merge] calls [Vdom.Attr.many] *)
  type t =
    | Legacy_do_not_merge
    | Merge
end

(** [create] returns a typeahead using native browser controls.

    [to_option_description] if provided will render the description provided below the
    option.

    [unboxed] will emit the typeahead (and its associated datalist) as a [fragment] (to be
    embedded in a parent node) rather than as a self-contained [div].

    Note that [set_selected] does not enforce that the given value is present in
    [all_options]. Setting a value not in [all_options] will successfully set the
    typeahead to that value. *)

val create
  :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
  -> ?placeholder:string Bonsai.t
  -> ?on_select_change:('a option -> unit Ui_effect.t) Bonsai.t
  -> ?to_string:('a -> string) Bonsai.t
  -> ?to_option_description:('a -> string) Bonsai.t
  -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
  -> ?attr_merge_behavior:Attr_merge_behavior.t
  -> ?unboxed:unit
  -> sexp_of:('a -> Sexp.t)
  -> equal:('a -> 'a -> bool)
  -> all_options:'a list Bonsai.t
  -> local_ Bonsai.graph
  -> 'a option t Bonsai.t

val create_multi
  :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
  -> ?extra_pills_container_attrs:Vdom.Attr.t list Bonsai.t
  -> ?pills_tab_behavior:[ `Prevent_tabbing | `Tab_with_index of int ] Bonsai.t
  -> ?placeholder:string Bonsai.t
  -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Bonsai.t
  -> ?to_string:('a -> string) Bonsai.t
  -> ?to_option_description:('a -> string) Bonsai.t
  -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
  -> ?unboxed:unit
  -> ?split:(string -> string list)
  -> ?attr_merge_behavior:Attr_merge_behavior.t
  -> ('a, 'cmp) Comparator.Module.t
  -> all_options:'a list Bonsai.t
  -> local_ Bonsai.graph
  -> ('a, 'cmp) Set.t t Bonsai.t

module Private : sig
  module For_testing : sig
    val create_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?placeholder:string Bonsai.t
      -> ?on_select_change:('a option -> unit Ui_effect.t) Bonsai.t
      -> ?to_string:('a -> string) Bonsai.t
      -> ?to_option_description:('a -> string) Bonsai.t
      -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
      -> ?attr_merge_behavior:Attr_merge_behavior.t
      -> ?unboxed:unit
      -> sexp_of:('a -> Sexp.t)
      -> equal:('a -> 'a -> bool)
      -> all_options:'a list Bonsai.t
      -> local_ Bonsai.graph
      -> 'a option t Bonsai.t

    val create_multi_with_browser_behavior_in_test
      :  ?extra_attrs:Vdom.Attr.t list Bonsai.t
      -> ?extra_pills_container_attrs:Vdom.Attr.t list Bonsai.t
      -> ?pills_tab_behavior:[ `Prevent_tabbing | `Tab_with_index of int ] Bonsai.t
      -> ?placeholder:string Bonsai.t
      -> ?on_set_change:(('a, 'cmp) Set.t -> unit Ui_effect.t) Bonsai.t
      -> ?to_string:('a -> string) Bonsai.t
      -> ?to_option_description:('a -> string) Bonsai.t
      -> ?handle_unknown_option:(string -> 'a option) Bonsai.t
      -> ?unboxed:unit
      -> ?split:(string -> string list)
      -> ?attr_merge_behavior:Attr_merge_behavior.t
      -> ('a, 'cmp) Comparator.Module.t
      -> all_options:'a list Bonsai.t
      -> local_ Bonsai.graph
      -> ('a, 'cmp) Set.t t Bonsai.t
  end
end
