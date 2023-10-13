open Objlang
open TypeError
open Debug
open TypeContext

(** types for various environments *)
module Env = Map.Make (String)

(** map<String, Objlng.type> *)
type tenv = typ Env.t

(** map<String, function_def<unit>> *)
type fenv = unit function_def Env.t

(** map<String, class_def<unit>> *)
type senv = unit class_def Env.t

let length_of = List.length

(** compare([te.annot], [t]) -> [te.annot] *)
let check te t =
  if te.annot <> t then
    raise
      (UnexpectedTypeError {expected= Definition t; actual= Definition te.annot})
  else te


(** Checks if [t] is of the a class type.

    If true, calls 'true_handler' with the class name as an argument.

    Otherwise, raises an 'UnexpectedTypeError' using the 'behaviour' argument to
    detail the expected type.

    @return what true handler returns *)
let require_class t true_handler behaviour =
  match t with
  | TClass name ->
      true_handler name
  | other_t ->
      raise
        (UnexpectedTypeError
           {expected= Behavior behaviour; actual= Definition other_t} )


(** Check if [t] is array. if is the case apply handler on the element type of
    this array *)
let require_array t true_handler =
  match t with
  | TArray t ->
      true_handler t
  | other_t ->
      raise
        (UnexpectedTypeError
           {expected= Behavior Indexable; actual= Definition other_t} )


let method_of_class name class_def =
  match
    List.find_opt
      (fun (def : untyped_function) -> def.name = name)
      class_def.methods
  with
  | Some d ->
      d
  | None ->
      raise (UndefinedError (Method (class_def.name, name)))


let is_same_type e1 e2 = e1.annot = e2.annot

(** [l]: List<Tuple<String, E>>; [env]: Map<String, E> [return]: Map<String, E> *)
let add2env l env = List.fold_left (fun env (x, t) -> Env.add x t env) env l

let remove_from_env l env =
  List.fold_left (fun env (x, t) -> Env.remove x env) env l


let rec type_expr (context : type_context) (e : untyped_expression) :
    typed_expression =
  let inject f = f context in
  let find_variable = inject find_variable in
  let find_function = inject find_function in
  let find_class = inject find_class in
  let find_method = inject find_method in
  let rec_call = type_expr context in
  match e.expr with
  | Cst n ->
      mk_expr TInt (Cst n)
  | Bool b ->
      mk_expr TBool (Bool b)
  | Var x ->
      mk_expr (find_variable x) (Var x)
  | Binop (op, e1, e2) ->
      let typed_e1 = rec_call e1 and typed_e2 = rec_call e2 in
      let op_t = match op with Add -> TInt | Mul -> TInt | Lt -> TBool in
      if is_same_type typed_e1 typed_e2 then
        mk_expr op_t (Binop (op, typed_e1, typed_e2))
      else
        raise
          (UnexpectedTypeError
             { expected= Definition typed_e1.annot
             ; actual= Definition typed_e2.annot } )
  | Call (name, args) ->
      let fd = find_function name in
      let params = fd.params in
      (* check arguments quantity *)
      let argc = length_of args and paramc = length_of params in
      if argc <> paramc then
        raise (MissingArgumentError {expected= paramc; actual= argc})
      else
        (* check each argument type *)
        let typed_args =
          check_expr_list args (List.map (fun (_, t) -> t) params) context
        in
        (* typed the call by return type *)
        mk_expr fd.return (Call (name, typed_args))
  | New (classname, args) ->
      let constructor_def =
        find_method {classname; methodname= "constructor"}
      in
      let params_t = List.map (fun (_, t) -> t) constructor_def.params in
      let typed_args = check_expr_list args params_t context in
      mk_expr (TClass classname) (New (classname, typed_args))
  | NewTab (t, size) ->
      (* check [size] is of type Int *)
      let typed_size = check (rec_call size) TInt in
      mk_expr (TArray t) (NewTab (t, typed_size))
  | Read memeory ->
      let mem_t, typed_mem = type_mem context memeory in
      mk_expr mem_t (Read typed_mem)
  | This ->
      let this_t = find_variable this_variable_name in
      mk_expr this_t This
  | MCall (obj, method_name, args) ->
      let typed_obj = rec_call obj in
      let obj_t = typed_obj.annot in
      let class_def = require_class obj_t find_class (HasMethod method_name) in
      let method_def = method_of_class method_name class_def in
      let return_t = method_def.return in
      let typed_args =
        check_expr_list args
          (List.map (fun (_, t) -> t) method_def.params)
          context
      in
      mk_expr return_t (MCall (rec_call obj, method_name, typed_args))


and type_mem (context : type_context) (m : unit mem) =
  let inject f = f context in
  let find_class = inject find_class in
  match m with
  | Arr (id, index) ->
      (* check [index] is of type Int *)
      let typed_index = check (type_expr context index) TInt in
      (* check [id] is an array that can be indexed *)
      let typed_array = type_expr context id in
      require_array typed_array.annot (fun t ->
          (t, Arr (typed_array, typed_index)) )
  | Atr (id, field) ->
      let typed_id = type_expr context id in
      let type_attribute_access class_name =
        let class_def = find_class class_name in
        let fields = class_def.fields in
        let field_t =
          match List.find_opt (fun (fid, ft) -> fid = field) fields with
          | Some (fid, ft) ->
              ft
          | None ->
              raise (UndefinedError (Attribute (class_name, field)))
        in
        (field_t, Atr (typed_id, field))
      in
      require_class typed_id.annot type_attribute_access (HasField field)


and check_expr_list expressions types context =
  List.map2 (fun expr t -> check (type_expr context expr) t) expressions types


let rec type_seq context (f : untyped_function) (s : untyped_sequence) =
  List.map (type_instr context f) s


and type_instr (context : type_context) (f : untyped_function)
    (instruction : untyped_instruction) =
  let inject f = f context in
  let find_variable = inject find_variable in
  let type_expr = inject type_expr in
  let type_seq = type_seq context f in
  match instruction with
  | Putchar e ->
      Putchar (check (type_expr e) TInt)
  | Set (id, e) ->
      let id_t = find_variable id in
      let typed_e = type_expr e in
      Set (id, check typed_e id_t)
  | If (c, b1, b2) ->
      let typed_c = check (type_expr c) TBool in
      If (typed_c, type_seq b1, type_seq b2)
  | While (c, b) ->
      let typed_c = check (type_expr c) TBool in
      While (typed_c, type_seq b)
  | Return e ->
      let return_t = f.return in
      Return (check (type_expr e) return_t)
  | Expr e ->
      Expr (type_expr e)
  | Write (mem, e) ->
      let mem_t, typed_mem = type_mem context mem in
      Write (typed_mem, check (type_expr e) mem_t)


let type_function context f =
  let context = add_variables context f.locals in
  let context = add_variables context f.params in
  {f with code= type_seq context f f.code}


let type_class context (c : untyped_class) : typed_class =
  (* inject [this] and [attributes] as varilables *)
  let context = add_variable context this_variable_name (TClass c.name) in
  let context = add_variables context c.fields in
  (* type class methods *)
  {c with methods= List.map (type_function context) c.methods}


(** Type and check a program type every constructs of a program and perform type
    checking on each construct. Exit and report the error when meeting an error. *)
let type_program (p : untyped_program) : typed_program =
  let context = make_empty_context () in
  let context = add_classes context p.classes in
  let context = add_functions context p.functions in
  let context = add_variables context p.globals in
  { p with
    functions= List.map (type_function context) p.functions
  ; classes= List.map (type_class context) p.classes }
