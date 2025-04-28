open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(** The purpose of this module is to generate PRTs with various configurations so that we
    can compare them in benchmarks, comparison reports, and tests. *)

module All_apis_configs = All_apis_configs
module Col_dependency_configs = Col_dependency_configs

(** {2 Utils} *)

module Sharable = Sharable
module Symbol_table = Symbol_table
