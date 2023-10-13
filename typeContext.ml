open Objlang

(** types for various environments *)
module Env = Map.Make (String)

type type_context =
  { varilables: typ Env.t
  ; functions: untyped_function Env.t
  ; classes: untyped_class Env.t }

let make_empty_context () =
  {varilables= Env.empty; functions= Env.empty; classes= Env.empty}


let add_varilable context id t =
  {context with varilables= Env.add id t context.varilables}


let remove_varilable context id =
  {context with varilables= Env.remove id context.varilables}


let add_function context id f =
  {context with functions= Env.add id f context.functions}


let remove_function context id =
  {context with functions= Env.remove id context.functions}


let add_class context id c = {context with classes= Env.add id c context.classes}

let remove_class context id =
  {context with classes= Env.remove id context.classes}
