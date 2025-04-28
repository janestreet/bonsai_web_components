open! Core
open! Import

(** https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate *)

val make
  :  ('input, 'unparsed Product.Same.t) Bonsai_arrow_deprecated.t
  -> parse:('unparsed -> 'parsed Or_error.t)
  -> unparse:('parsed -> 'unparsed)
  -> ('input, 'parsed Product.Errorable.Same.t) Bonsai_arrow_deprecated.t

val make_via_string
  :  (module Stringable with type t = 'parsed)
  -> ('input, string Product.Same.t) Bonsai_arrow_deprecated.t
  -> ('input, 'parsed Product.Errorable.Same.t) Bonsai_arrow_deprecated.t
