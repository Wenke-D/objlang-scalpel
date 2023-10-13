open Objlang
open TypeError
open Debug

module Env = Map.Make (String)
(** types for various environments *)

type tenv = typ Env.t
(** map<String, Objlng.type> *)

type fenv = unit function_def Env.t
(** map<String, function_def<unit>> *)

type senv = unit class_def Env.t
(** map<String, class_def<unit>> *)

let length_of = List.length

(**
    compare([te.annot], [t]) -> [te.annot]
*)
let check te t =
  if te.annot <> t then
    raise
      (UnexpectedTypeError
         { expected = Definition t; actual = Definition te.annot })
  else te

(** Checks if [t] is of the a class type.

    If true, calls 'true_handler' with the class name as an argument.
  
    Otherwise, raises an 'UnexpectedTypeError' 
    using the 'behaviour' argument to detail the expected type.

    @return what true handler returns
*)
let require_class t true_handler behaviour =
  match t with
  | TClass name -> true_handler name
  | other_t ->
      raise
        (UnexpectedTypeError
           { expected = Behavior behaviour; actual = Definition other_t })

(** 
  Check if [t] is array.
  if is the case apply handler on the element type of this array
*)
let require_array t true_handler =
  match t with
  | TArray t -> true_handler t
  | other_t ->
      raise
        (UnexpectedTypeError
           { expected = Behavior Indexable; actual = Definition other_t })

let method_of_class name class_def =
  match
    List.find_opt
      (fun (def : untyped_function) -> def.name = name)
      class_def.methods
  with
  | Some d -> d
  | None -> raise (UndefinedError (Method (class_def.name, name)))

let is_same_type e1 e2 = e1.annot = e2.annot

(** 
    [l]: List<Tuple<String, E>>;
    [env]: Map<String, E>
    [return]: Map<String, E>
    *)
let add2env l env = List.fold_left (fun env (x, t) -> Env.add x t env) env l

let remove_from_env l env =
  List.fold_left (fun env (x, t) -> Env.remove x env) env l

(* main typing function *)
let type_program (p : untyped_program) : typed_program =
  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty
  and fenv =
    add2env
      (List.map (fun (f : unit function_def) -> (f.name, f)) p.functions)
      Env.empty
  and cenv = add2env (List.map (fun s -> (s.name, s)) p.classes) Env.empty in

  let def_of_func (id : string) : untyped_function =
    match Env.find_opt id fenv with
    | Some def -> def
    | None -> raise (UndefinedError (Function id))
  and def_of_class (id : string) =
    match Env.find_opt id cenv with
    | Some def -> def
    | None -> raise (UndefinedError (Class id))
  and def_of_method (id : string) (clazz : untyped_class) =
    match
      List.find_opt (fun (fd : untyped_function) -> fd.name = id) clazz.methods
    with
    | Some def -> def
    | None -> raise (UndefinedError (Method (clazz.name, id)))
  in
  (* typing a function definition *)
  let type_fdef (fdef : untyped_function) : typed_function =
    (* inject local varilables *)
    let tenv = add2env fdef.locals tenv in
    (* inject parameters as local varilables *)
    let tenv = add2env fdef.params tenv in
    let type_of_var (id : string) =
      match Env.find_opt id tenv with
      | Some t -> t
      | None -> raise (UndefinedError (Variable id))
    in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e : untyped_expression) : typed_expression =
      match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x -> mk_expr (type_of_var x) (Var x)
      | Binop (op, e1, e2) ->
          let typed_e1 = type_expr e1 and typed_e2 = type_expr e2 in
          let op_t = match op with Add -> TInt | Mul -> TInt | Lt -> TBool in
          if is_same_type typed_e1 typed_e2 then
            mk_expr op_t (Binop (op, typed_e1, typed_e2))
          else
            raise
              (UnexpectedTypeError
                 {
                   expected = Definition typed_e1.annot;
                   actual = Definition typed_e2.annot;
                 })
      | Call (name, args) ->
          let fd = def_of_func name in
          let params = fd.params in
          (* check arguments quantity *)
          let argc = length_of args and paramc = length_of params in
          if argc <> paramc then
            raise (MissingArgumentError { expected = paramc; actual = argc })
          else
            (* check each argument type *)
            let typed_args =
              check_expr_list args (List.map (fun (_, t) -> t) params)
            in
            (* typed the call by return type *)
            mk_expr fd.return (Call (name, typed_args))
      | New (class_name, args) ->
          let class_def = def_of_class class_name in
          let constructor_def = def_of_method "constructor" class_def in
          let params_t = List.map (fun (_, t) -> t) constructor_def.params in
          let typed_args = check_expr_list args params_t in
          mk_expr (TClass class_name) (New (class_name, typed_args))
      | NewTab (t, size) ->
          (* check [size] is of type Int *)
          let typed_size = check (type_expr size) TInt in
          mk_expr (TArray t) (NewTab (t, typed_size))
      | Read memeory ->
          let mem_t, typed_mem = type_mem memeory in
          mk_expr mem_t (Read typed_mem)
      | This ->
          let this_t = type_of_var this_variable_name in
          mk_expr this_t This
      | MCall (obj, method_name, args) ->
          let typed_obj = type_expr obj in
          let obj_t = typed_obj.annot in
          let class_def =
            require_class obj_t def_of_class (HasMethod method_name)
          in
          let method_def = method_of_class method_name class_def in
          let return_t = method_def.return in
          let typed_args =
            check_expr_list args (List.map (fun (_, t) -> t) method_def.params)
          in
          mk_expr return_t (MCall (type_expr obj, method_name, typed_args))
    and type_mem m =
      (* Return (element_type, typed_memory_access) *)
      match m with
      | Arr (id, index) ->
          (* check [index] is of type Int *)
          let typed_index = check (type_expr index) TInt in
          (* check [id] is an array that can be indexed *)
          let typed_array = type_expr id in
          require_array typed_array.annot (fun t ->
              (t, Arr (typed_array, typed_index)))
      | Atr (id, field) ->
          let typed_id = type_expr id in
          let type_attribute_access class_name =
            let class_def = def_of_class class_name in
            let fields = class_def.fields in
            let field_t =
              match List.find_opt (fun (fid, ft) -> fid = field) fields with
              | Some (fid, ft) -> ft
              | None -> raise (UndefinedError (Attribute (class_name, field)))
            in
            (field_t, Atr (typed_id, field))
          in
          require_class typed_id.annot type_attribute_access (HasField field)
    and check_expr_list expressions types =
      List.map2 (fun expr t -> check (type_expr expr) t) expressions types
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e -> Putchar (check (type_expr e) TInt)
      | Set (id, e) ->
          let id_t = type_of_var id in
          let typed_e = type_expr e in
          Set (id, check typed_e id_t)
      | If (c, b1, b2) ->
          let typed_c = check (type_expr c) TBool in
          If (typed_c, type_seq b1, type_seq b2)
      | While (c, b) ->
          let typed_c = check (type_expr c) TBool in
          While (typed_c, type_seq b)
      | Return e ->
          let return_t = fdef.return in
          Return (check (type_expr e) return_t)
      | Expr e -> Expr (type_expr e)
      | Write (mem, e) ->
          let mem_t, typed_mem = type_mem mem in
          Write (typed_mem, check (type_expr e) mem_t)
    in
    { fdef with code = type_seq fdef.code }
  in
  let type_cdef (cdef : untyped_class) : typed_class =
    (* inject [this] and [attributes] as varilables *)
    let tenv = Env.add this_variable_name (TClass cdef.name) tenv in
    let tenv = add2env cdef.fields tenv in
    (* type class methods *)
    let result = { cdef with methods = List.map type_fdef cdef.methods } in
    (* remove [this] and [attributes] from varilables *)
    let tenv = Env.remove this_variable_name tenv in
    let _ = remove_from_env cdef.fields tenv in
    result
  in
  {
    p with
    functions = List.map type_fdef p.functions;
    classes = List.map type_cdef p.classes;
  }
