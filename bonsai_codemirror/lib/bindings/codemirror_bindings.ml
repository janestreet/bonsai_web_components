open! Core
include Custom_ojs_converter
include For_ppx

module type State_effect_spec = sig
  type t
end

module State = struct
  include For_ppx.State

  module State_effect = struct
    type t = State.state_effect

    let is = State.State_effect.is
    let value t ~type_ = if is t ~type_ then Some (State.State_effect.value t) else None

    let define (type a) (module S : State_effect_spec with type t = a)
      : a State.State_effect_type.t
      =
      (* As noted in the mli, this module isn't really needed, it's just here to ensure
         that `type t` is a real type. *)
      ignore (module S : State_effect_spec);
      State.State_effect.define ()
    ;;

    let reconfigure = State.State_effect.reconfigure
  end

  module Range_set_update_spec = struct
    include Range_set_update_spec

    module Filter_spec = struct
      type 'v t =
        { f : from:int -> to_:int -> value:'v -> bool
        ; filter_from : int
        ; filter_to : int
        }
    end

    let create ~add ~sort ~(filter : 'v Filter_spec.t option) =
      match filter with
      | Some filter ->
        create
          ~add
          ~sort
          ~filter:(Some filter.f)
          ~filter_from:(Some filter.filter_from)
          ~filter_to:(Some filter.filter_to)
      | None -> create ~add ~sort ~filter:None ~filter_from:None ~filter_to:None
    ;;
  end

  module Range_set = struct
    include Range_set

    let between t ~from ~to_ ~f =
      between t ~from ~to_ ~f:(fun ~from ~to_ ~value ->
        match f ~from ~to_ ~value with
        | `Stop -> Some false
        | `Continue -> None)
    ;;
  end
end

module Gutter = struct
  include Gutter

  module Gutter_marker_type = struct
    open Js_of_ocaml

    let define
      (type input)
      ~(to_dom : input -> View.Editor_view.t -> Dom_html.element Js.t)
      ?(destroy : (input -> Dom_html.element Js.t -> unit) option)
      ?(equal : (input -> input -> bool) option)
      ?element_class
      ()
      =
      (* Ok not to unwrap parameters, because they will stay in the OCaml representation. *)
      let to_dom = Js.wrap_callback to_dom in
      let equal =
        Option.map equal ~f:(fun equal ->
          (fun a b -> equal a b |> Js.bool) |> Js.wrap_callback)
        |> Js.Opt.option
      in
      let destroy = Option.map destroy ~f:Js.wrap_callback |> Js.Opt.option in
      let element_class = Option.map element_class ~f:Js.string |> Js.Opt.option in
      let x =
        Js.Unsafe.fun_call
          (Js.Unsafe.js_expr
             {|function(toDom, equal, destroy, elementClass) {

      const CustomGutterMarker = class extends codemirror.View.GutterMarker {
          constructor(ocamlInput) {
            super();
            this.ocamlInput = ocamlInput;
          }

          toDOM(editorView) {
            return toDom(this.ocamlInput, editorView);
          }

          eq(other) {
            if (equal) {
              return equal(this.ocamlInput, other.ocamlInput);
            }
            return super.eq(other);
          }

          destroy(node) {
            if (destroy) {
              destroy(this.ocamlInput, node);
            }
            return super.destroy(node);
          }
        };

        if (elementClass) {
          CustomGutterMarker.prototype.elementClass = elementClass;
        }

        return CustomGutterMarker;
      }|})
          [| Js.Unsafe.inject to_dom
           ; Js.Unsafe.inject equal
           ; Js.Unsafe.inject destroy
           ; Js.Unsafe.inject element_class
          |]
      in
      Staged.stage (fun input ->
        new%js x input |> (Obj.magic : _ Js.t -> Gutter_marker.t))
    ;;
  end
end
