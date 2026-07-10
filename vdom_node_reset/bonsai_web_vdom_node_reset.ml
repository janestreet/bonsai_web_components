open! Core
module Vdom = Virtual_dom.Vdom

module Reset_attrs = struct
  let common =
    {%css|
      @layer base {
        box-sizing: border-box;
        margin: 0;
        padding: 0;
        border: 0 solid;
      }
    |}
  ;;

  let hr =
    {%css|
      @layer base {
        height: 0;
        color: inherit;
        border-top-width: 1px;
      }
    |}
  ;;

  let abbr =
    {%css|
      @layer base {
        -webkit-text-decoration: underline dotted;
        text-decoration: underline dotted;
      }
    |}
  ;;

  let heading =
    {%css|
      @layer base {
        font-size: inherit;
        font-weight: inherit;
      }
    |}
  ;;

  let a =
    {%css|
      @layer base {
        color: inherit;
        -webkit-text-decoration: inherit;
        text-decoration: inherit;
      }
    |}
  ;;

  let table =
    {%css|
      @layer base {
        text-indent: 0;
        border-color: inherit;
        border-collapse: collapse;
      }
    |}
  ;;

  (* ol, ul, menu *)
  let list =
    {%css|
      @layer base {
        list-style: none;
      }
    |}
  ;;

  (* img, svg, video, canvas, audio, iframe, embed, object *)
  let replaced_element =
    {%css|
      @layer base {
        display: block;
        vertical-align: middle;
      }
    |}
  ;;

  let img_and_video =
    {%css|
      @layer base {
        max-width: 100%;
        height: auto;
      }
    |}
  ;;

  (* button, input, select, optgroup, textarea *)
  let field =
    {%css|
      @layer base {
        font: inherit;
        font-feature-settings: inherit;
        font-variation-settings: inherit;
        letter-spacing: inherit;
        color: inherit;
        border-radius: 0;
        background-color: transparent;
        opacity: 1;

        &.for-testing--force-focus-visible {
          /* This reproduces the user-agent behaviour when the element is :focus-visible. */
          outline-style: auto;
        }
      }
    |}
  ;;

  let button =
    {%css|
      @layer base {
        cursor: pointer;

        &:disabled {
          cursor: default;
        }
      }
    |}
  ;;

  (* input, textarea *)
  let placeholder =
    {%css|
      @layer base {
        opacity: 1;
        color: #9ca3af;
      }
    |}
  ;;
end

let wrap' fn ~reset_attrs ?key ?(attrs = []) children =
  let attrs = Some (Vdom.Attr.many reset_attrs :: attrs) in
  fn ?key ?attrs children
;;

let wrap_childless' fn ~reset_attrs ?key ?(attrs = []) =
  let attrs = Some (Vdom.Attr.many reset_attrs :: attrs) in
  fn ?key ?attrs
;;

let wrap = wrap' ~reset_attrs:[ Reset_attrs.common ]
let wrap_childless = wrap_childless' ~reset_attrs:[ Reset_attrs.common ]

module Node = struct
  include Vdom.Node

  let a = wrap' ~reset_attrs:Reset_attrs.[ common; a ] a
  let abbr = wrap' ~reset_attrs:Reset_attrs.[ common; abbr ] abbr
  let address = wrap address
  let area = wrap area
  let article = wrap article
  let aside = wrap aside
  let audio = wrap' ~reset_attrs:Reset_attrs.[ common; replaced_element ] audio
  let b = wrap b
  let bdi = wrap bdi
  let bdo = wrap bdo
  let blockquote = wrap blockquote
  let body = wrap body
  let br = wrap_childless br
  let button = wrap' ~reset_attrs:Reset_attrs.[ common; field; button ] button
  let canvas = wrap' ~reset_attrs:Reset_attrs.[ common; replaced_element ] canvas
  let caption = wrap caption
  let cite = wrap cite
  let code = wrap code
  let col = wrap_childless col
  let colgroup = wrap colgroup
  let data = wrap data
  let datalist = wrap datalist
  let dd = wrap dd
  let del = wrap del
  let details = wrap details
  let dfn = wrap dfn
  let dialog = wrap dialog
  let div = wrap div
  let dl = wrap dl
  let dt = wrap dt
  let em = wrap em
  let fieldset = wrap fieldset
  let figcaption = wrap figcaption
  let figure = wrap figure
  let footer = wrap footer
  let form = wrap form
  let h1 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h1
  let h2 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h2
  let h3 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h3
  let h4 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h4
  let h5 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h5
  let h6 = wrap' ~reset_attrs:Reset_attrs.[ common; heading ] h6
  let head = wrap head
  let header = wrap header
  let hgroup = wrap hgroup
  let hr = wrap_childless' ~reset_attrs:Reset_attrs.[ common; hr ] hr
  let html = wrap html
  let iframe = wrap' ~reset_attrs:Reset_attrs.[ common; replaced_element ] iframe

  let img =
    wrap_childless'
      ~reset_attrs:Reset_attrs.[ common; img_and_video; replaced_element ]
      img
  ;;

  let input =
    wrap_childless' ~reset_attrs:Reset_attrs.[ common; field; placeholder ] input
  ;;

  let ins = wrap ins
  let kbd = wrap kbd
  let label = wrap label
  let legend = wrap legend
  let li = wrap li
  let map = wrap map
  let mark = wrap mark
  let main = wrap main
  let menu = wrap menu
  let meter = wrap meter
  let nav = wrap nav
  let noscript = wrap noscript
  let ol = wrap' ~reset_attrs:Reset_attrs.[ common; list ] ol
  let optgroup = wrap' ~reset_attrs:Reset_attrs.[ common; field ] optgroup
  let option = wrap option
  let output = wrap output
  let p = wrap p
  let picture = wrap picture
  let pre = wrap pre
  let progress = wrap progress
  let q = wrap q
  let rp = wrap rp
  let rt = wrap rt
  let ruby = wrap ruby
  let samp = wrap samp
  let search = wrap search
  let section = wrap section
  let select = wrap' ~reset_attrs:Reset_attrs.[ common; field ] select
  let slot = wrap slot
  let source = wrap source
  let small = wrap small
  let span = wrap span
  let strong = wrap strong
  let sub = wrap sub
  let summary = wrap summary
  let sup = wrap sup
  let table = wrap' ~reset_attrs:Reset_attrs.[ common; table ] table
  let tbody = wrap tbody
  let td = wrap td
  let template = wrap template
  let textarea = wrap' ~reset_attrs:Reset_attrs.[ common; field; placeholder ] textarea
  let tfoot = wrap tfoot
  let th = wrap th
  let thead = wrap thead
  let time = wrap time
  let tr = wrap tr
  let track = wrap_childless track
  let ul = wrap' ~reset_attrs:Reset_attrs.[ common; list ] ul
  let var = wrap var

  let video =
    wrap' ~reset_attrs:Reset_attrs.[ common; img_and_video; replaced_element ] video
  ;;

  let wbr = wrap_childless wbr
end

module Node0 = Node

module Html_syntax = struct
  include Virtual_dom.Vdom.Html_syntax.Html_syntax

  module Node = struct
    include Node
    include Node0
  end
end
