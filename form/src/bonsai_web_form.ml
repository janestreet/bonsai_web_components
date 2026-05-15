open! Core

(** A library for building forms in Bonsai.

    For most forms, you're better off hand-rolling with plain Bonsai primitives
    ([Bonsai.state], [let%arr], etc.) rather than using this library. Hand-rolled forms
    are simpler, more flexible, and avoid the mandatory [Or_error.t] wrapping and
    boilerplate of the [Form.t] type.

    This library is most useful when you have a form that significantly benefits from
    automatic type-driven generation (e.g. large records via [Typed.Record.make], or
    variants via [Typed.Variant.make]) and you care less about precisely controlling the
    form's layout. If you truly want an automatically generated form, consider using the
    even easier [Bonsai_web_auto_generated] library. *)

(** If you do use this library, [With_manual_view] is the recommended API. You get full
    control over how you'd like to combine views. Historically, [With_automatic_view] was
    the default, but it is quite restrictive and hard to customize. *)

module With_manual_view = struct
  include Form_manual

  module Elements = struct
    include Elements_manual
    include Typed_elements_manual
  end

  module Typed = Typed_manual

  module Private = struct
    include Elements_manual.Private
  end
end

(** [With_automatic_view] forms have their views composed in an opinionated way. This used
    to be the default and recommended way to build forms in Bonsai, but now people should
    prefer [With_manual_view] where possible. *)
module With_automatic_view = struct
  include Form_automatic
  module Elements = Elements_automatic
  module Typed = Typed_automatic
end
