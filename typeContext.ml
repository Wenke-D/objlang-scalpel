open Objlang

(** types for various environments *)
module Env = Map.Make (String)

type type_context =
  { varilables: typ Env.t
  ; functions: untyped_function Env.t
  ; classes: untyped_class Env.t }
