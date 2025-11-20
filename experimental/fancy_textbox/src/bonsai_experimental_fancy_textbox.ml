open! Core
open! Virtual_dom.Vdom.Html_syntax
module Vdom = Virtual_dom.Vdom

module Style =
  [%css
  stylesheet
    {|
      .wrapper {
        /* shrink the textbox to fit the size of the text */
        width: fit-content;
        height: fit-content;

        /* overflow shouldn't happen, but if it does,
           allow the user to still interact with it. */
        overflow: visible;

        /* The <textarea> and <pre> elements are absolutely
           positioned, so make the wrapper position:relative
           so that they're absolute with respect to the wrapper.  */
        position: relative;
      }

      .textarea,
      .pre {
        /* The textbox and pre elements need to share ~all of their
           properties so that they line up correctly.  Inherit all
           these properties from the parent to allow the user to
           style the component by styling a container node. */
        all: inherit;

        /* Regardless of the parents values, we want to revert
           these properties back to the defaults. */
        background: transparent;
        display: block;
        border-radius: 0;
        margin: 0;
        padding: 0;
        border: 0;

        /* don't inherit width/height from parent node */
        width: unset;
        height: unset;
      }

      .pre {
        /* overflow shouldn't be possible due to the wrapper
           node growing to fit the content, but if something unexpected
           happens, a visible overflow is preferred. */
        overflow: visible;

        /* The user can select things from the textbox, so prevent
           them from selecting things from the <pre> */
        user-select: none;
      }

      .textarea {
        /* make the text invisible */
        color: transparent;

        /* The text being invisible means that the caret will
           also be invisible unless we set it to something */
        caret-color: var(--caret-color);

        /* This node is absolutely positioned, so it can't influence
           the size of the wrapper node.  Clip it! */
        overflow: clip;

        /* fill the wrapper node */
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
      }

      .textarea,
      .pre,
      .pre > * {
        /* pre-wrap means that whitespace isn't auto-collapsed,
           but it can still wrap long lines if necessary */
        white-space: pre-wrap;
      }
    |}]

let filler = {%html|<span style="color: transparent">.</span>|}

module Expert = struct
  let create
    ?(min_width = (`Ch 12.0 : Css_gen.Length.t))
    ?(caret_color = (`Name "black" : Css_gen.Color.t))
    ~text
    ~set_text
    ~process
    ()
    =
    let min_width = Css_gen.Length.to_string_css min_width in
    let caret_color = Css_gen.Color.to_string_css caret_color in
    let textbox =
      Vdom_input_widgets.Entry.text_area
        ~extra_attrs:[ Style.textarea ]
        ~value:text
        ~on_input:set_text
        ()
    in
    let pre_content =
      (* <pre> nodes have some really weird rules about when they remove whitespace from
         the start and end of the string. We add some invisible '.' characters to fill
         prevent them from removing any whitespace, which ensures that the textbox and
         <pre> elements line up *)
      let should_prepend =
        match String.lsplit2 ~on:'\n' text with
        | Some (first_line, _) -> String.for_all first_line ~f:Char.is_whitespace
        | None -> String.for_all text ~f:Char.is_whitespace
      in
      let should_append = String.is_suffix text ~suffix:"\n" in
      let prepend = if should_prepend then Some filler else None
      and append = if should_append then Some filler else None in
      {%html|<pre %{Style.pre}>?{prepend}*{process text}?{append}</pre>|}
    in
    {%html|
      <div
        %{Style.wrapper}
        %{Style.Variables.set_all ~caret_color}
        style="min-width: %{min_width}"
      >
        %{pre_content} %{textbox}
      </div>
    |}
  ;;
end

module Decoration = struct
  (* Currently we only support changing the color of text. For monospace fonts, we could
     also allow changing the font weight and italics. *)
  type t = { color : Css_gen.Color.t option } [@@deriving sexp_of]

  let create ?color () = { color }

  let to_attr { color } =
    Option.value_map color ~default:Vdom.Attr.empty ~f:(fun color ->
      Vdom.Attr.style (Css_gen.color color))
  ;;
end

module Substring : sig
  (* [Core.Substring] is a module for strings that are cheaply slicable.

     By comparison, [String.sub s ~pos ~len] makes a _copy_ of [s] between the regions
     [pos] and [len], while [Substring.sub s ~pos ~len] will contain a _pointer_ to s,
     along with its offset and length.

     This distinction is important for the fancy-textbox because with [String.t] repeated
     calls to [split_pred] would allocate O(n^2) memory, but with [Substring.t], it only
     allocates O(n).

     I'm shadowing the [Core.Substring] module here in order to only expose the functions
     that are useful in this file. *)

  type t

  val of_string : string -> t
  val to_string : t -> string
  val is_prefix : t -> prefix:string -> bool
  val chop_prefix : t -> prefix:string -> t

  (** splits the substring into two segments:
      1. A string containing all the characters [c] for which [f c] returns true in
         sequence. The first time [f c] returns false, the iteration is over and [f] won't
         be called again.
      2. A substring containing the rest of the string. *)
  val split_pred : t -> f:(char -> bool) -> string * t
end = struct
  type t = Substring.t

  let of_string s =
    (* Initial creation of substring does require a full copy *)
    Substring.create (Bytes.of_string s)
  ;;

  let to_string t = Substring.to_string t

  let equal_to_string a b =
    match Substring.length a, String.length b with
    | length_of_a, length_of_b when length_of_a <> length_of_b -> false
    | the_length, _ ->
      let rec loop i =
        if i >= the_length
        then true
        else if Char.equal (Substring.get a i) (String.get b i)
        then loop (i + 1)
        else false
      in
      loop 0
  ;;

  let is_prefix t ~prefix =
    let chopped = Substring.prefix t (String.length prefix) in
    equal_to_string chopped prefix
  ;;

  let take_while t ~f =
    let length_of_t = Substring.length t in
    let rec loop i =
      if i >= length_of_t
      then length_of_t
      else if f (Substring.get t i)
      then loop (i + 1)
      else i
    in
    Substring.to_string (Substring.sub t ~len:(loop 0))
  ;;

  let chop_prefix t ~prefix = Substring.drop_prefix t (String.length prefix)

  let split_pred s ~f =
    let prefix = take_while s ~f in
    let rest = chop_prefix s ~prefix in
    prefix, rest
  ;;
end

let create_error ~i ~prefix ~text ~decoration =
  Error.create_s
    [%message
      "Error applying decoration!"
        ~at_char:(i : int)
        ~expected_prefix:prefix
        ~but_found:text
        (decoration : Decoration.t)]
;;

(* Translates a list of decorated strings into a list of vdom nodes. Used in the
   non-expert `create` function. *)
let lower ~text ~decorated =
  let decorated =
    (* remove all "whitespace-only" decorated regions and remove whitespace from the start
       and end of decorated regions *)
    List.filter_map decorated ~f:(fun (s, a) ->
      match String.strip s with
      | "" -> None
      | s -> Some (s, a))
  in
  let accumulated, decorated =
    (* fold through the list of decorated regions, generating vdom nodes for each
       decorated region. *)
    List.fold_map
      decorated
      (* The accumulator is a pair, containing
         1. The remaining raw text to process
         2. The character offset into the currently parsed string

         It's wrapped in an [Or_error.t] because the user could provide decorated regions
         that don't overlap with the actual source text, so we need to stop parsing at
         that point. *)
      ~init:(Ok (Substring.of_string text, 0))
      ~f:(fun text (decorated_text, decoration) ->
        match text with
        | Error e ->
          (* If we're in an error state, propagate that *)
          Error e, []
        | Ok (remaining_text_including_whitespace, i) ->
          let leading_whitespace, text =
            (* strip out the leading whitespace, but keep track of the whitespace so that
               we can re-insert it later. *)
            Substring.split_pred remaining_text_including_whitespace ~f:Char.is_whitespace
          in
          let i = i + String.length leading_whitespace in
          (match Substring.is_prefix text ~prefix:decorated_text with
           | false ->
             (* if the decorated text doesn't match what we're expecting, then return an
                error *)
             ( Error
                 ( create_error
                     ~i
                     ~prefix:decorated_text
                     ~text:(Substring.to_string text)
                     ~decoration
                 , remaining_text_including_whitespace )
             , [] )
           | true ->
             let decorated =
               {%html|<span %{Decoration.to_attr decoration}>#{ decorated_text }</span>|}
             in
             let leading_whitespace =
               if String.is_empty leading_whitespace
               then None
               else Some (Vdom.Node.text leading_whitespace)
             in
             let text =
               (* drop the decorated text from remaining text to process *)
               Substring.chop_prefix text ~prefix:decorated_text
             in
             let i = i + String.length decorated_text in
             Ok (text, i), List.filter_opt [ leading_whitespace; Some decorated ]))
  in
  let remaining_text =
    match accumulated with
    | Error (e, remaining_text) ->
      eprint_s [%message "" ~_:(e : Error.t)];
      remaining_text
    | Ok (remaining_text, _) -> remaining_text
  in
  let remaining_text =
    match Substring.to_string remaining_text with
    | "" -> []
    | remaining_text -> [ Vdom.Node.text remaining_text ]
  in
  List.concat (List.append decorated [ remaining_text ])
;;

let create
  ?min_width
  ?caret_color
  ~text
  ~set_text
  ~(process : string -> (string * Decoration.t) list)
  ()
  =
  Expert.create () ?min_width ?caret_color ~text ~set_text ~process:(fun text ->
    let decorated = process text in
    lower ~text ~decorated)
;;
