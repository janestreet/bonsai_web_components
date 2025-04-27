open! Core
open! Bonsai_web

let arrow_helper ~attrs ~arrow_len () =
  let (`Px_float arrow_len) = arrow_len in
  let side_len = `Px_float (arrow_len *. sqrt 2.) in
  Vdom.Node.div
    ~attrs:
      ([%css
         {|
           height: %{side_len#Css_gen.Length};
           width: %{side_len#Css_gen.Length};
           transform: rotate(45deg);
           border-bottom: none;
           border-right: none;
         |}]
       :: attrs)
    []
;;

module Tooltip = struct
  type t =
    { tooltip_attrs : Vdom.Attr.t list
    ; anchor_attrs : Vdom.Attr.t list
    ; main_axis_offset : float
    ; cross_axis_offset : float
    ; show_delay : Time_ns.Span.t option
    ; hide_grace_period : Time_ns.Span.t option
    ; hoverable_hide_grace_period : Time_ns.Span.t
    ; arrow : Vdom.Node.t option
    }

  let default_anchor_attrs =
    let module Style =
      [%css
      stylesheet
        {|
          @layer bonsai_web_ui_view.tooltip_defaults {
            .anchor {
              text-decoration: underline;
              text-decoration-style: dotted;
              text-underline-offset: 0.15em;
            }
          }
        |}]
    in
    Style.anchor
  ;;

  let create
    ~tooltip_attrs
    ~arrow
    ?(anchor_attrs = [ default_anchor_attrs ])
    ?(main_axis_offset = 0.)
    ?(cross_axis_offset = 0.)
    ?show_delay
    ?hide_grace_period
    ?(hoverable_hide_grace_period = Time_ns.Span.of_ms 150.)
    ()
    =
    { tooltip_attrs
    ; anchor_attrs
    ; main_axis_offset
    ; cross_axis_offset
    ; show_delay
    ; hide_grace_period
    ; hoverable_hide_grace_period
    ; arrow
    }
  ;;
end

module Popover = struct
  type t =
    { popover_attrs : Vdom.Attr.t list
    ; default_main_axis_offset : float
    ; default_main_axis_offset_with_arrow : float
    ; arrow : Vdom.Node.t
    }

  let create
    ~popover_attrs
    ~arrow
    ?(default_main_axis_offset = 0.)
    ?(default_main_axis_offset_with_arrow = 12.)
    ()
    =
    { popover_attrs
    ; default_main_axis_offset
    ; default_main_axis_offset_with_arrow
    ; arrow
    }
  ;;
end

module Modal = struct
  type t = { modal_attrs : Vdom.Attr.t list }
end
