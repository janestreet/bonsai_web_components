open! Core
open! Bonsai_web

(** Override a [Vdom.Node.t] in test. In non-test environments this function is a no-op.
    In test environments, rather than rendering the node it renders a custom node named
    after the source file. This can help when testing nodes that are noisy when printed.

    Parameters:
    - [?override_attrs] - render custom attrs instead of the node's attrs.
    - [?override_content] - provide custom content inside the node (default empty). *)
val override_for_test
  :  here:[%call_pos]
  -> ?override_attrs:Vdom.Attr.t list
  -> ?override_content:(unit -> Vdom.Node.t)
  -> Vdom.Node.t
  -> Vdom.Node.t
