open! Core
open Bonsai_web

(** A textbox with suggested results, modeled off of Chrome's address bar.

    - "Enter" invokes [on_select] with the focused suggestion. If the suggestion list is
      closed, the callback is not invoked, because of course nothing is focused; instead
      the suggestion list is opened.
    - "Clicking" on the suggestion list invokes [on_select].
    - "Escape" or unfocusing the text input closes the suggestion list
    - "Tab" and "Down Arrow" move the focused item down. If the suggestion list is closed,
      it gets opened, and the focused item is set to the top item.
    - "Shift-Tab" and "Up Arrow" move the focused item up. Again, if the list is closed,
      it gets opened, but focus is sent to the bottom item.
    - Editing the textbox content re-filters the items.

    Notably, there is a difference between "selection" and "focus". Selection is an effect
    that runs when the user clicks on an item, or presses enter. Focus is a styling
    mechanism, allowing the user to keyboard-navigate through the suggestion list.

    Only [max_visible_items] are rendered at once, so even with thousands of suggestions,
    DOM updates should be very quick. *)

module Suggestion_list_kind : sig
  type t =
    | Transient_overlay
    (** When set to Transient, the suggestion-list only shows up when the textbox is
        focused, and the list will float over other items on the page. *)
    | Permanent_fixture
    (** Permanent will make the suggestion-list always present, and it will take up space
        during the layout of the application. *)
    | Expert
    (** Expert does not assign [position: relative] to the parent container, so the user
        must implement the anchor *)
  [@@deriving sexp, compare, enumerate, equal]
end

module Expand_direction : sig
  type t =
    | Down
    (** Autocomplete options appear below the textbox, with the first item at the top of
        the list. *)
    | Up
    (** Autocomplete options appear above the textbox, with the first item at the bottom
        of the list. *)
  [@@deriving sexp, compare, enumerate, equal]
end

module On_focus : sig
  type t =
    | Focus_first_item
    (** Focuses the the first item in the suggestion list. If [suggestion_list_kind] is
        [Transient_overlay] this will also cause the suggestion list to appear as soon as
        the textbox is focused. *)
    | Do_nothing
    (** Do nothing until the user starts typing, or presses up/down. If
        [suggestion_list_kind] is [Transient_overlay] the suggestion list will remain
        hidden until the user begins typing. *)
  [@@deriving sexp, compare, enumerate, equal]
end

module On_hover_item : sig
  type t =
    | Do_nothing
    (** Do nothing when the mouse hovers over an item in the suggestion list. This is the
        recommended behavior, since you can style hovered items with the CSS `:hover`
        selector. *)
    | Focus_hovered_item
    (** This is not recommended, because users typically do not expect hovering to change
        application state. *)
  [@@deriving sexp, compare, enumerate, equal]
end

type 'k t =
  { focused_item : 'k option
  ; view : Vdom.Node.t
  ; query : string
  ; set_query : ?close_list:bool -> string -> unit Effect.t
  (** [close_list = true] prevents the list from opening up after setting the query. Not
      entirely certain why it defaults to opening the list as it doesn't seem like a
      desireable behavior, but keeping behavior as-is in order to maintain backwards
      compat. *)
  ; focus_input : unit Effect.t
  ; activate_for_benchmarking : unit Effect.t
  (** The querybox doesn't do any work until it is "initialized", but that only happens on
      the focus, keydown, and input DOM events, which we don't simulate in benchmarks. *)
  }
[@@deriving fields ~getters]

val create
  :  ('k, 'cmp) Comparator.Module.t
  -> ?initial_query:string
  -> ?max_visible_items:int Bonsai.t
       (** The value defaults to Transient. Read doc comment on the type for more info. *)
  -> ?suggestion_list_kind:Suggestion_list_kind.t Bonsai.t
       (** The value defaults to Down. Read doc comment on the type for more info. *)
  -> ?expand_direction:Expand_direction.t Bonsai.t
       (** If provided, the attributes in this value will be attached to the vdom node
           representing the currently focused item in the list. *)
  -> ?on_focus:On_focus.t Bonsai.t
       (** The value defaults to Focus_first_item. Read doc comment on the type for more
           info. *)
  -> ?on_hover_item:On_hover_item.t Bonsai.t
       (** The value defaults to [Do_nothing]. We recommend using [:hover] CSS to style
           hovered querybox suggestions. *)
  -> ?focused_item_attr:Vdom.Attr.t Bonsai.t
       (** If provided, [extra_list_container_attr] will be added to the vdom node
           containing the list of suggestions. *)
  -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
       (** If provided, [extra_input_attr] will be added to the query text input. *)
  -> ?extra_input_attr:Vdom.Attr.t Bonsai.t
       (** If provided [extra_attr] will be added to the outermost div of this component. *)
  -> ?extra_attr:Vdom.Attr.t Bonsai.t
       (** If provided [on_blur] will be called whenever a blur triggered outside of the
           query box (including both input and item list) occurs. *)
  -> ?on_blur:unit Ui_effect.t Bonsai.t
       (** The value defaults to [None] which does nothing. This is so that the user can
           either reset/clear/modify the input text on blur. This only fires when focus
           shifts to something other than the input or the selection list *)
  -> ?modify_input_on_blur:(string -> string Ui_effect.t) option Bonsai.t
       (** The value defaults to [fun focused_key query -> ""], which resets the input box
           whenever the user selects an option. The text inside of the input box will
           become the result of this function. *)
  -> ?modify_input_on_select:('k -> string -> string) Bonsai.t
       (** [f] generates the set of completion options by returning a map from a
           user-provided key ['k] to the view for that element in the dropdown list. The
           currently entered filter from the textbox part of the component is provided as
           an argument to the function and it is expected that you use this to do your own
           filtering and return the filtered map. *)
  -> ?ignore_tab_key:bool
       (** when [ignore_tab_key] is true, [Tab] will select a next focusable element on a
           page instead of selecting a next item in the drop down *)
  -> f:(string Bonsai.t -> local_ Bonsai.graph -> ('k, Vdom.Node.t, 'cmp) Map.t Bonsai.t)
       (** [on_select] is called when [enter] is hit, or an item is clicked. *)
  -> on_select:('k -> unit Effect.t) Bonsai.t
  -> unit
  -> local_ Bonsai.graph
  -> 'k t Bonsai.t

module Filter_strategy : sig
  type t =
    | Fuzzy_match
    | Fuzzy_search_and_score
  [@@deriving compare, enumerate, equal, sexp_of]
end

(** [stringable] is like [create] but takes a map with possible completion options,
    instead of a function to generate them. Items are filtered and sorted based on
    [filter_strategy].

    - [Fuzzy_match]: uses the [fuzzy_match] library to only display items that match the
      current pattern. Items are displayed in the order they appear in the input map.

    - [Fuzzy_search_and_score]: uses the [fuzzy_search] library to only display items that
      match the current pattern. It also scores how well each item matches in order to
      sort the matching items. *)
val stringable
  :  ('k, 'cmp) Comparator.Module.t
  -> ?initial_query:string
  -> ?max_visible_items:int Bonsai.t
  -> ?suggestion_list_kind:Suggestion_list_kind.t Bonsai.t
  -> ?expand_direction:Expand_direction.t Bonsai.t
  -> ?on_focus:On_focus.t Bonsai.t
  -> ?on_hover_item:On_hover_item.t Bonsai.t
  -> ?focused_item_attr:Vdom.Attr.t Bonsai.t
  -> ?extra_list_container_attr:Vdom.Attr.t Bonsai.t
  -> ?extra_input_attr:Vdom.Attr.t Bonsai.t
  -> ?extra_attr:Vdom.Attr.t Bonsai.t
  -> ?to_view:('k -> string -> Vdom.Node.t)
       (** The value defaults to [None] which does nothing. This is so that the user can
           either reset/clear/modify the input text on blur. This only fires when focus
           shifts to something other than the input or the selection list *)
  -> ?modify_input_on_blur:(string -> string Ui_effect.t) option Bonsai.t
       (** The value defaults to [`Reset], which resets the input box whenever the user
           selects an option. [`Don't_change] leaves the input unchanged and
           [`Autocomplete] sets the input to the selected string. *)
  -> ?modify_input_on_select:[ `Reset | `Don't_change | `Autocomplete ] Bonsai.t
  -> filter_strategy:Filter_strategy.t
  -> on_select:('k -> unit Effect.t) Bonsai.t
  -> ('k, string, 'cmp) Map.t Bonsai.t
  -> local_ Bonsai.graph
  -> 'k t Bonsai.t

module Collate_map_with_score : sig
  module Scored_key : sig
    type 'k t = int * 'k

    include Comparator.Derived with type 'a t := 'a t

    module M (T : Comparator.S) : sig
      type nonrec t = T.t t [@@deriving sexp_of]

      include
        Comparator.S
        with type t := t
         and type comparator_witness = T.comparator_witness comparator_witness
    end

    module Map : sig
      type nonrec ('k, 'v, 'cmp) t = ('k t, 'v, 'cmp comparator_witness) Map.t
    end
  end

  (** [collate] sorts and filters the input map according to a [score] function (filtering
      a result out is done by returning 0 from [score], and transforms the data in the map
      according to a [to_result] function.

      The performance trade-off is specific: we assume that the input map doesn't change
      very often so that we can pre-process all the items in the map ahead of time. Thus,
      the goal of this function is not to be as incremental as possible, but rather to
      have as low of constants as possible.

      [query_is_as_strict] is used to determine whether a new query will filter out at
      least as many items as the previous query. If so, then we can skip running [score]
      on items that have already been filtered away.

      Each time the query changes, we remember a previous query and which items have been
      filtered away by that query. However, if the previous query limits the input to
      fewer than [stop_trimming_input_at_count], then we don't update the previous query.
      This allows the user to specify that when the number of results is small enough that
      it is more worthwhile to cache a more general query in order to allow backtracking
      through previous queries without abandoning caching altogether. *)
  val collate
    :  (module Comparator.S with type t = 'k and type comparator_witness = 'cmp)
    -> preprocess:(key:'k -> data:'v -> 'preprocessed)
    -> score:('query -> 'preprocessed -> int)
    -> query_is_as_strict:('query -> as_:'query -> bool)
    -> to_result:('preprocessed -> key:'k -> data:'v -> 'result)
    -> ('k, 'v, 'cmp) Map.t Bonsai.t
    -> 'query Bonsai.t
    -> local_ Bonsai.graph
    -> ('k, 'result, 'cmp) Scored_key.Map.t Bonsai.t
end
