open Objlang
open TypeError

(** types for various environments *)
module Env = Map.Make (String)

type type_context =
  { variables: typ Env.t
  ; functions: untyped_function Env.t
  ; classes: untyped_class Env.t }

let make_empty_context () =
  {variables= Env.empty; functions= Env.empty; classes= Env.empty}


let add_variable context id t =
  {context with variables= Env.add id t context.variables}


let reduce_list context op l = List.fold_left op context l

let remove_variable context id =
  {context with variables= Env.remove id context.variables}


let find_variable context id =
  match Env.find_opt id context.variables with
  | Some t ->
      t
  | None ->
      raise (UndefinedError (Variable id))


let add_function context id f =
  {context with functions= Env.add id f context.functions}


let remove_function context id =
  {context with functions= Env.remove id context.functions}


let find_function context id =
  match Env.find_opt id context.functions with
  | Some f ->
      f
  | None ->
      raise (UndefinedError (Function id))


let add_class context id c = {context with classes= Env.add id c context.classes}

let remove_class context id =
  {context with classes= Env.remove id context.classes}


let find_class context id =
  match Env.find_opt id context.classes with
  | Some f ->
      f
  | None ->
      raise (UndefinedError (Class id))


type method_identifier = {classname: string; methodname: string}

let find_method context id =
  let class_def = find_class context id.classname in
  match
    List.find_opt
      (fun (def : untyped_function) -> def.name = id.methodname)
      class_def.methods
  with
  | Some d ->
      d
  | None ->
      raise (UndefinedError (Method (class_def.name, id.methodname)))


let add_variables context =
  reduce_list context (fun ctx (id, t) -> add_variable ctx id t)


let add_functions context =
  reduce_list context (fun ctx (def : untyped_function) ->
      add_function ctx def.name def )


let add_classes context =
  reduce_list context (fun ctx (def : untyped_class) ->
      add_class ctx def.name def )


let bind_1 arg f = f arg
