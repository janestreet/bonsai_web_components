open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module State = struct
  type 'a t =
    { current : 'a
    ; set : 'a -> unit Ui_effect.t
    }
  [@@deriving fields ~getters ~iterators:create]

  let create = Fields.create
end

module Result = struct
  type 'a t =
    { tabs : Vdom.Node.t list
    ; current : 'a
    }

  let combine_trivially { tabs; current } =
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_container" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_tabs" ] tabs
      ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "bonsai_ui_tab_body" ] [ current ]
      ]
  ;;
end

let tab_state ?sexp_of ?equal ~initial (local_ graph) =
  let current, set = Bonsai.state initial ?sexp_of_model:sexp_of ?equal graph in
  let%arr current and set in
  State.create ~current ~set
;;

let tab_ui
  ?decorate
  ?additional_button_attributes
  ~sexp_of
  ~all_tabs
  ~equal
  state
  ~f
  (local_ graph)
  =
  let default_additional_button_attributes () =
    Bonsai.return (fun ~is_selected:_ _ -> Vdom.Attr.empty)
  in
  let default_decorate sexp_of_t =
    Bonsai.return (fun t -> t |> sexp_of_t |> Sexp.to_string_hum |> Vdom.Node.text)
  in
  let decorate = Option.value decorate ~default:(default_decorate sexp_of) in
  let additional_button_attributes =
    Option.value
      additional_button_attributes
      ~default:(default_additional_button_attributes ())
  in
  let tab_to_button =
    let%map state and decorate and additional_button_attributes in
    fun kind ->
      let is_selected = equal kind (State.current state) in
      let selected_attr =
        if is_selected then Vdom.Attr.class_ "selected" else Vdom.Attr.empty
      in
      let attr =
        Vdom.Attr.(
          on_click (fun _ -> State.set state kind)
          @ selected_attr
          @ class_ "bonsai_ui_tab"
          @ name (kind |> sexp_of |> Sexp.to_string_mach)
          @ additional_button_attributes ~is_selected kind)
      in
      Vdom.Node.button ~attrs:[ attr ] [ decorate kind ]
  in
  let tabs =
    let%map all_tabs and tab_to_button in
    List.map all_tabs ~f:tab_to_button
  in
  let result =
    let change_tab = state >>| State.set in
    let current = state >>| State.current in
    f ~change_tab current graph
  in
  let%arr current = result
  and tabs in
  { Result.tabs; current }
;;
