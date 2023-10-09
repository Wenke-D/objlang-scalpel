open Objlng
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

(* expression *)
type untyped_expr = unit expression
type typed_expr = typ expression

(* function *)
type untyped_func = unit function_def
type typed_func = typ function_def

(* class *)
type untyped_class = unit class_def
type typed_class = typ class_def

(* program *)
type untyped_prog = unit program
type typed_prog = typ program

let length_of = List.length

(**
    compare([te.annot], [t]) -> [te.annot]
*)
let check (te : typed_expr) t =
  if te.annot <> t then
    raise
      (UnexpectedTypeError
         { expected = Definition t; actual = Definition te.annot })
  else te

let is_same_type (e1 : typed_expr) (e2 : typed_expr) = e1.annot = e2.annot

(** 
    [l]: List<Tuple<String, E>>;
    [env]: Map<String, E>
    [return]: Map<String, E>
    *)
let add2env l env = List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let type_program (p : untyped_prog) : typed_prog =
  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty
  and fenv =
    add2env
      (List.map (fun (f : unit function_def) -> (f.name, f)) p.functions)
      Env.empty
  and cenv = add2env (List.map (fun s -> (s.name, s)) p.classes) Env.empty in
  let type_of_var (id : string) =
    match Env.find_opt id tenv with
    | Some t -> t
    | None -> raise (UndefinedError (Variable id))
  and def_of_func (id : string) : untyped_func =
    match Env.find_opt id fenv with
    | Some def -> def
    | None -> raise (UndefinedError (Function id))
  and def_of_class (id : string) =
    match Env.find_opt id cenv with
    | Some def -> def
    | None -> raise (UndefinedError (Class id))
  in
  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e : untyped_expr) : typed_expr =
      match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x -> mk_expr (Env.find x tenv) (Var x)
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
          let fd = Env.find name fenv in
          let params = fd.params in
          (* check arguments quantity *)
          let argc = length_of args and paramc = length_of params in
          if argc <> paramc then
            raise (MissingArgumentError { expected = paramc; actual = argc })
          else
            (* check each argument type *)
            let typed_args =
              List.map2
                (fun arg (name, param_t) -> check (type_expr arg) param_t)
                args params
            in
            (* typed the call by return type *)
            mk_expr fd.return (Call (name, typed_args))
      | New (class_name, constructor) ->
          let _ = def_of_class class_name in
          mk_expr (TClass class_name)
            (New (class_name, List.map type_expr constructor))
      | NewTab (t, size) ->
          (* check [size] is of type Int *)
          let typed_size = check (type_expr size) TInt in
          mk_expr (TArray t) (NewTab (t, typed_size))
      | Read memeory ->
          let mem_t, typed_mem = type_mem memeory in
          mk_expr mem_t (Read typed_mem)
      | This ->
          let this_t = type_of_var "this" in
          mk_expr this_t This
      | MCall (obj, method_name, args) ->
          let typed_obj = type_expr obj in
          let obj_t = typed_obj.annot in
          let class_def : untyped_class =
            match obj_t with
            | TClass name -> def_of_class name
            | other ->
                raise
                  (UnexpectedTypeError
                     {
                       expected = Behavior (HasMethod method_name);
                       actual = Definition other;
                     })
          in
          let method_def = def_of_func method_name in
          let return_t = method_def.return in
          mk_expr return_t
            (MCall (type_expr obj, method_name, List.map type_expr args))
    and type_mem m =
      (* Return (element_type, typed_memory_access) *)
      match m with
      | Arr (id, index) -> (
          (* check [index] is of type Int *)
          let typed_index = check (type_expr index) TInt in
          (* check [id] is an array that can be indexed *)
          let typed_array = type_expr id in
          match typed_array.annot with
          | TArray t -> (t, Arr (typed_array, typed_index))
          | other_type ->
              raise
                (UnexpectedTypeError
                   {
                     expected = Behavior Indexable;
                     actual = Definition other_type;
                   }))
      | Atr (id, field) -> (
          let typed_id = type_expr id in
          (* check [id] is of type structure *)
          match typed_id.annot with
          | TClass s ->
              let class_def = def_of_class s in
              let fields = class_def.fields in
              let field_t =
                match List.find_opt (fun (fid, ft) -> fid = field) fields with
                | Some (fid, ft) -> ft
                | None -> raise (UndefinedError (Attribute (s, field)))
              in
              (field_t, Atr (typed_id, field))
          | t ->
              raise
                (UnexpectedTypeError
                   {
                     expected = Behavior (HasField field);
                     actual = Definition t;
                   }))
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e -> Putchar (check (type_expr e) TInt)
      | Set (id, e) -> (
          match Env.find_opt id tenv with
          | None -> failwith (Printf.sprintf "%s is not defined" id)
          | Some id_t ->
              let typed_e = type_expr e in
              Set (id, check typed_e id_t))
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
    (* TODO: inject [this] as varilable *)
    (* TODO: inject fields as varilable *)
    (* TODO: inject method as functions *)
    { cdef with methods = List.map type_fdef cdef.methods }
  in
  {
    p with
    functions = List.map type_fdef p.functions;
    classes = List.map type_cdef p.classes;
  }
