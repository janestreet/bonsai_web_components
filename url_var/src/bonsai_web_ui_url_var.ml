open! Core
open! Bonsai_web
module History = Html5_history.Opinionated

let get_uri () =
  let open Js_of_ocaml in
  Dom_html.window##.location##.href |> Js.to_string |> Uri.of_string
;;

module Components = struct
  type t =
    { path : string
    ; query : string list String.Map.t
    ; fragment : string option
    }
  [@@deriving sexp, equal]

  let create ?(path = "") ?(query = String.Map.empty) ?(fragment = None) () =
    { path; query; fragment }
  ;;

  let empty = create ()

  let to_path_and_query { path; query; fragment } =
    let uri = get_uri () in
    uri
    |> Fn.flip Uri.with_path path
    |> Fn.flip Uri.with_query (Map.to_alist query)
    |> Fn.flip Uri.with_fragment fragment
  ;;

  let of_uri uri =
    let path = Uri.path uri |> String.chop_prefix_if_exists ~prefix:"/" in
    let query =
      uri
      |> Uri.query
      |> String.Map.of_alist_multi
      |> Map.filter_map ~f:(function
        | [ value ] -> Some value
        | _ -> None)
    in
    let fragment = Uri.fragment uri in
    { path; query; fragment }
  ;;
end

module type T = sig
  type t [@@deriving sexp, equal]
end

module type S = sig
  include T

  val parse_exn : Components.t -> t
  val unparse : t -> Components.t
end

module type S_via_sexp = sig
  type t [@@deriving sexp, equal]
end

module Literally_just_a_gigantic_sexp (M : S_via_sexp) : S with type t = M.t = struct
  include M

  let query_param_name = "query"

  let parse_exn { Components.query; _ } =
    Map.find_exn query query_param_name |> List.hd_exn |> Sexp.of_string |> [%of_sexp: t]
  ;;

  let unparse t =
    let uri = get_uri () in
    let param = Sexp.to_string ([%sexp_of: t] t) in
    { Components.path = Uri.path uri
    ; query = String.Map.singleton query_param_name [ param ]
    ; fragment = Uri.fragment uri
    }
  ;;
end

module Original_components = Components

let reload_without_intercepting =
  let reload =
    Js_of_ocaml.Js.Unsafe.js_expr
      {js|
    (function(on_finished) {
      window.navigation.reload({
        info: {
          bypass_intercept: true
        }
      }).finished.then(on_finished);
    })
    |js}
  in
  Bonsai.Effect.Expert.of_fun ~f:(fun ~callback ~on_exn:_ ->
    let wrapped_callback = Js_of_ocaml.Js.Unsafe.callback callback in
    Js_of_ocaml.Js.Unsafe.fun_call
      reload
      [| Js_of_ocaml.Js.Unsafe.inject wrapped_callback |])
;;

let can_bypass_intercept =
  let check_can_bypass =
    Js_of_ocaml.Js.Unsafe.js_expr {js| ((event) => !!(event.info?.bypass_intercept)) |js}
  in
  fun event ->
    let js_bool =
      Js_of_ocaml.Js.Unsafe.fun_call
        check_can_bypass
        [| Js_of_ocaml.Js.Unsafe.inject event |]
    in
    Js_of_ocaml.Js.to_bool js_bool
;;

type 'a t =
  | Browser of
      { var : 'a Bonsai.Expert.Var.t
      ; history : 'a History.t
      }
  | In_nodejs_test of
      { var : 'a Bonsai.Expert.Var.t
      ; sexp_of : 'a -> Sexp.t
      }

let get_var = function
  | Browser { var; _ } -> var
  | In_nodejs_test { var; _ } -> var
;;

let get_global =
  let get = Js_of_ocaml.Js.Unsafe.js_expr {js|(function () { return globalThis }) |js} in
  fun () -> Js_of_ocaml.Js.Unsafe.fun_call get [||]
;;

let listen_to_navigation_events_exn ~parse_exn ~f =
  let open Js_of_ocaml in
  (* We have to retrieve this value fresh every time in case the user is mocking the
     browser implementation in tests multiple times
  *)
  let navigation = (Js.Unsafe.coerce (get_global ()))##.navigation in
  let (_ : _ Js.t) =
    navigation##addEventListener
      (Js.string "navigate")
      (Dom_html.handler (fun event ->
         let can_intercept =
           Js.to_bool event##.canIntercept
           && (not (Js.Opt.test event##.downloadRequest))
           && not (can_bypass_intercept event)
         in
         let try_intercept () =
           let value =
             Js.to_string event##.destination##.url
             |> Uri.of_string
             |> Original_components.of_uri
             |> parse_exn
           in
           (match am_running_how with
            | `Node_jsdom_test | `Node_test ->
              Core.print_endline "Intercepted navigation event"
            | _ -> ());
           event##intercept
             (Js.Unsafe.obj
                [| ( "handler"
                   , Js.Unsafe.inject
                       (Js.wrap_callback (fun () ->
                          f value;
                          Js.undefined)) )
                 ; "focusReset", Js.Unsafe.inject (Js.string "manual")
                |])
         in
         if can_intercept
         then (
           try try_intercept () with
           | _ -> ());
         Js.bool true))
  in
  ()
;;

let listen_to_navigation_events ~parse_exn ~f =
  (* We wrap this call in a try-catch until the API is finalized as per
     https://html.spec.whatwg.org/#navigation-api *)
  try listen_to_navigation_events_exn ~parse_exn ~f with
  | _ ->
    (match am_running_how with
     | `Browser
     | `Browser_test
     | `Browser_benchmark
     | `Node
     | `Node_benchmark
     | `Node_test -> ()
     | `Node_jsdom_test ->
       (* Only JSDom tests can have the intercept handler actually attached. *)
       print_s
         [%message
           "Unable to attach intercept handler in tests. If you want to test this \n\
           \ functionality, please call \
            [For_testing.mock_browser_functionality_for_tests] \n\
           \  \n\
           \            before creating the URL var."])
;;

let set ?(how : [ `Push | `Replace ] option) t a =
  let var = get_var t in
  (* We need to make sure the bonsai var is set _before_ we update the history. Otherwise
     the [listen_to_navigation_events] callback will observe the new value before it's
     updated in the var which causes an infinite loop of history updates. *)
  Bonsai.Expert.Var.set var a;
  let how = Option.value how ~default:`Push in
  match t with
  | Browser { history; _ } ->
    (match how with
     | `Push -> History.update history a
     | `Replace -> History.replace history a)
  | In_nodejs_test { sexp_of; _ } ->
    (* In tests, we don't interact with the [History] API and instead just log the value *)
    (match how with
     | `Push -> print_s [%message "Pushing to history" ~new_location:(sexp_of a : Sexp.t)]
     | `Replace ->
       print_s
         [%message
           "Replacing current location in history" ~new_location:(sexp_of a : Sexp.t)])
;;

let maybe_add_navigation_listener (type a) (module S : S with type t = a) ~navigation t =
  match navigation with
  | `Ignore -> ()
  | `Intercept ->
    (* At the point where we intercept a navigation event the URL / histroy has already
       updated, so we don't want to duplicate the navigation entry.

       Instead we can replace the current entry with a new one that carries a payload
       generated based on the parsed new URL.

       See
       https://developer.mozilla.org/en-US/docs/Web/API/NavigateEvent/intercept#examples *)
    listen_to_navigation_events ~parse_exn:S.parse_exn ~f:(fun next_page ->
      let is_current_page = S.equal next_page (Bonsai.Expert.Var.get (get_var t)) in
      if not is_current_page then set ~how:`Replace t next_page)
;;

let create_exn'
  (type a)
  ?(navigation = `Intercept)
  (module S : S with type t = a)
  ~on_bad_uri
  =
  match am_running_how with
  | `Browser | `Browser_test | `Browser_benchmark ->
    let module Uri_routing = struct
      include S

      let parse uri =
        let components = Components.of_uri uri in
        match parse_exn components with
        | a -> Ok a
        | exception e ->
          eprint_s [%message "couldn't parse uri" (components : Components.t) (e : exn)];
          Error `Not_found
      ;;

      let to_path_and_query uri = Components.to_path_and_query (unparse uri)
    end
    in
    let module History_state = struct
      type uri_routing = a

      include S

      include Binable.Of_sexpable_with_uuid (struct
          include S

          let caller_identity =
            Bin_prot.Shape.Uuid.of_string "918e794b-02c3-4f27-ad86-3f406a41fc4b"
          ;;
        end)

      let to_uri_routing = Fn.id
      let of_uri_routing = Fn.id
    end
    in
    let t =
      History.init_exn
        ~log_s:(ignore : Sexp.t -> unit)
        (module History_state)
        (module Uri_routing)
        ~on_bad_uri
    in
    let value = History.current t in
    let var = Bonsai.Expert.Var.create value in
    Bus.subscribe_permanently_exn (History.changes_bus t) ~f:(Bonsai.Expert.Var.set var);
    let t = Browser { var; history = t } in
    maybe_add_navigation_listener ~navigation (module S) t;
    t
  | `Node | `Node_benchmark | `Node_test | `Node_jsdom_test ->
    (match on_bad_uri with
     | `Raise ->
       let error_message =
         [%string
           "Error: [Bonsai_web_ui_url_var.create_exn] requires a fallback value to work \
            within a \n\
            nodejs environment because it relies on the browser's history API. This is \
            stubbed \n\
            out in tests, but we still require a default in order to initialize the url \
            var."]
       in
       failwith error_message
     | `Default_state default ->
       let t =
         In_nodejs_test { var = Bonsai.Expert.Var.create default; sexp_of = S.sexp_of_t }
       in
       maybe_add_navigation_listener ~navigation (module S) t;
       t)
;;

let create_exn (type a) (module S : S with type t = a) ~fallback =
  create_exn' (module S) ~navigation:`Ignore ~on_bad_uri:(`Default_state fallback)
;;

let value t =
  let var = get_var t in
  Bonsai.Expert.Var.value var
;;

let incr t = Ui_incr.Var.watch (Bonsai.Expert.Var.incr_var (get_var t))

let update ?how t ~f =
  get_var t
  |> Bonsai.Expert.Var.update ~f:(fun old ->
    let new_ = f old in
    set ?how t new_;
    new_)
;;

let get t = get_var t |> Bonsai.Expert.Var.get
let set_effect ?how t = Effect.of_sync_fun (fun a -> set ?how t a)

let update_effect ?how url_var ~f =
  Effect.of_sync_fun (fun () -> update ?how url_var ~f) ()
;;

type 'a url_var = 'a t

module Typed = struct
  module Components = struct
    include Uri_parsing.Components

    let slash_regexp = Re.Str.regexp "/"
    let unicode_slash_regexp = Re.Str.regexp "%2F"

    let sanitize_slashes s =
      let url_unicode_slash = "%2F" in
      Re.Str.global_replace slash_regexp url_unicode_slash s
    ;;

    let parse_unicode_slashes s = Re.Str.global_replace unicode_slash_regexp "/" s

    let of_original_components
      ?(encoding_behavior : Uri_parsing.Percent_encoding_behavior.t = Correct)
      ?trailing_slash_behavior
      (original : Components.t)
      =
      let split_path =
        match original.path with
        | "" -> []
        | path ->
          (match encoding_behavior with
           | Legacy_incorrect ->
             String.split ~on:'/' path |> List.map ~f:parse_unicode_slashes
           | Correct -> decode_path ?trailing_slash_behavior path)
      in
      { Uri_parsing.Components.path = split_path
      ; query = original.query
      ; fragment = original.fragment
      }
    ;;

    let to_original_components
      ?(encoding_behavior : Uri_parsing.Percent_encoding_behavior.t = Correct)
      (typed_components : t)
      =
      { Components.path =
          (match encoding_behavior with
           | Legacy_incorrect ->
             String.concat ~sep:"/" (List.map typed_components.path ~f:sanitize_slashes)
           | Correct -> encode_path typed_components.path)
      ; query = typed_components.query
      ; fragment = typed_components.fragment
      }
    ;;
  end

  module Projection = Uri_parsing.Projection
  module Parser = Uri_parsing.Parser
  module Path_order = Uri_parsing.Path_order

  module Versioned_parser = struct
    include Uri_parsing.Versioned_parser

    let of_non_typed_parser
      ?encoding_behavior
      ?trailing_slash_behavior
      ~(parse_exn : Original_components.t -> 'a)
      ~(unparse : 'a -> Original_components.t)
      ()
      =
      let projection =
        let parse_exn components =
          parse_exn (Components.to_original_components ?encoding_behavior components)
        in
        let unparse result =
          Components.of_original_components
            ?encoding_behavior
            ?trailing_slash_behavior
            (unparse result)
        in
        { Projection.parse_exn; unparse }
      in
      Uri_parsing.Versioned_parser.of_non_typed_parser projection
    ;;
  end

  let make'
    (type a)
    (parser : a Uri_parsing.Versioned_parser.t)
    ?encoding_behavior
    ?trailing_slash_behavior
    ~(fallback : Exn.t -> Original_components.t -> a)
    ~on_fallback_raises
    ()
    =
    let projection = Uri_parsing.Versioned_parser.eval ?encoding_behavior parser in
    let try_with_backup ~f =
      try f () with
      | e -> Option.value_or_thunk on_fallback_raises ~default:(fun () -> raise e)
    in
    let parse_exn (components : Original_components.t) =
      try
        let typed_components =
          Components.of_original_components
            ?encoding_behavior
            ?trailing_slash_behavior
            components
        in
        let result : a Uri_parsing.Parse_result.t =
          projection.parse_exn typed_components
        in
        match result.remaining.path with
        | [] -> result.result
        | unparsed_path ->
          raise_s
            [%message "Part of the path was left unparsed!" (unparsed_path : string list)]
      with
      | e -> try_with_backup ~f:(fun () -> fallback e components)
    in
    let unparse (t : a) =
      let typed_components =
        projection.unparse
          { Uri_parsing.Parse_result.result = t
          ; remaining = Uri_parsing.Components.empty
          }
      in
      Components.to_original_components ?encoding_behavior typed_components
    in
    { Projection.parse_exn; unparse }
  ;;

  let make
    (type a)
    ?(navigation = `Intercept)
    ?on_fallback_raises
    ?encoding_behavior
    ?trailing_slash_behavior
    (module T : T with type t = a)
    (parser : a Uri_parsing.Versioned_parser.t)
    ~(fallback : Exn.t -> Original_components.t -> a)
    : a url_var
    =
    let projection =
      make'
        parser
        ?encoding_behavior
        ?trailing_slash_behavior
        ~fallback
        ~on_fallback_raises
        ()
    in
    let module S = struct
      include T

      let parse_exn = projection.parse_exn
      let unparse = projection.unparse
    end
    in
    let on_bad_uri =
      match am_running_how with
      | `Browser | `Browser_benchmark | `Browser_test -> `Raise
      | `Node | `Node_benchmark | `Node_test | `Node_jsdom_test ->
        (* Passing in some dummy values to [fallback] so that we can receive a default
           value in tests *)
        let default_value =
          fallback (Exn.create_s [%message "Dummy exception"]) Original_components.empty
        in
        `Default_state default_value
    in
    create_exn' (module S) ~navigation ~on_bad_uri
  ;;

  let make_projection
    (type a)
    ?on_fallback_raises
    ?encoding_behavior
    ?trailing_slash_behavior
    (parser : a Uri_parsing.Versioned_parser.t)
    ~(fallback : Exn.t -> Original_components.t -> a)
    =
    make'
      parser
      ?encoding_behavior
      ?trailing_slash_behavior
      ~fallback
      ~on_fallback_raises
      ()
  ;;

  module Value_parser = Uri_parsing.Value_parser

  let to_url_string (type a) ?encoding_behavior (parser : a Parser.t) a =
    let projection = Parser.eval ?encoding_behavior parser in
    let components =
      projection.unparse
        { Uri_parsing.Parse_result.result = a; remaining = Components.empty }
    in
    let with_query = Uri.add_query_params Uri.empty (Map.to_alist components.query) in
    let with_path = Uri.with_path with_query (String.concat ~sep:"/" components.path) in
    Uri.to_string with_path
  ;;
end

module For_testing = struct
  module Parse_result = Uri_parsing.Parse_result

  module Projection = struct
    type 'a t = (Typed.Components.t, 'a Parse_result.t) Uri_parsing.Projection.t

    let slash_regexp = Re.Str.regexp "/"
    let unicode_slash_regexp = Re.Str.regexp "%2F"

    let sanitize_slashes s =
      let url_unicode_slash = "%2F" in
      Re.Str.global_replace slash_regexp url_unicode_slash s
    ;;

    let parse_unicode_slashes s = Re.Str.global_replace unicode_slash_regexp "/" s

    let make ?encoding_behavior (parser : 'a Typed.Parser.t) =
      let projection = Typed.Parser.eval ?encoding_behavior parser in
      let parse_exn (components : Typed.Components.t) =
        projection.parse_exn
          { components with path = List.map ~f:parse_unicode_slashes components.path }
      in
      let unparse (result : 'a Parse_result.t) =
        let components = projection.unparse result in
        { components with path = List.map ~f:sanitize_slashes components.path }
      in
      { Uri_parsing.Projection.parse_exn; unparse }
    ;;

    let make_of_versioned_parser
      ?encoding_behavior
      (versioned_parser : 'a Typed.Versioned_parser.t)
      =
      Uri_parsing.Versioned_parser.eval ?encoding_behavior versioned_parser
    ;;

    let parse_exn (projection : 'a t) = projection.parse_exn
    let unparse (projection : 'a t) = projection.unparse
  end

  let mock_required_browser_functionality_for_navigation_intercept () =
    let () =
      Js_of_ocaml.Js.Unsafe.js_expr
        {js|
    (function() {
      const handlersForListener = {};
      const dispatchEvent = (event) => {
          (handlersForListener[event.type] || []).forEach(handler => {
            handler(event);
          });
      };

      globalThis.navigation = {
        ...globalThis.navigation,
        handlersForListener,
        addEventListener: (name, handler) => {
          globalThis.addEventListener(name, handler);
          handlersForListener[name] = handlersForListener[name] || [];
          handlersForListener[name].push(handler);
        },
        reload: (options) => {
          let event = {
            info: options?.info,
            canIntercept: true,
            downloadRequest: false,
            intercept: () => { throw new Error(); },
            destination: {
              url: globalThis.location.href
            },
          };
          return {
            committed: new Promise((resolve, reject) => resolve ()),
            finished: new Promise((resolve, reject) => {
              dispatchEvent(event);
              resolve();
            }),
          };
        },
        dispatchEvent
      };

      if (!globalThis.window) {
        globalThis.window = {}
      }
      globalThis.window.navigation = globalThis.navigation;

      // This is for JSDom
      if (window) {
        window.navigation = globalThis.navigation;
      }
    })()|js}
    in
    ()
  ;;
end
