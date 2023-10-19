open Debug
open Error_logic
open List_ext

let address_size = 4

module Input = struct
  include Objlang
end

module Output = struct
  include Imp
end

type input_expression = Input.typed_expression

type output_expression = Output.expression

let reduce = List.fold_left

let make_descriptor_name classname =
  Printf.sprintf "%s_descriptor_ptr" classname


let require_class t =
  match t with
  | Input.TClass name ->
      name
  | other_t ->
      raise (ImplementationError (Input.string_of_typ t))


let offset_of_method method_name (class_def : Input.typed_class) =
  let index = find_index class_def.methods (fun f -> f.name = method_name) in
  (index * address_size) + address_size


let tr_op : Input.binop -> Imp.binop = function
  | Add ->
      Add
  | Mul ->
      Mul
  | Lt ->
      Lt


(* main translation function *)
let translate_program (p : Input.typed_program) : Output.program =
  let find_class name =
    List.find (fun (def : Input.typed_class) -> def.name = name) p.classes
  in
  (* Size of a type in bytes. *)
  let rec sizeof (t : Input.typ) =
    match t with
    | TInt ->
        4
    | TBool ->
        4
    | TClass s ->
        let classdef = find_class s in
        (List.length classdef.fields * address_size) + address_size
        (* varilable of class type is only an address *)
    | TArray t ->
        sizeof
          t (* !!! size of a array is only the size of its element type !!! *)
    | TVoid ->
        4
  in
  let offset_of_array t (i : Input.typed_expression) : Input.typed_expression =
    let (offset : Input.typed_expression) =
      {expr= Input.Cst (sizeof t); annot= TInt}
    in
    {expr= Input.Binop (Input.Mul, i, offset); annot= TInt}
  in
  let offset_of_field (class_def : Input.typed_class) field :
      Input.typed_expression =
    let offset =
      partial_accum_excl class_def.fields
        (fun (name, t) -> sizeof t)
        (fun (name, t) -> name = field)
      + address_size
    in
    {expr= Cst offset; annot= TInt}
  in
  (* translation of an expression *)
  let rec tr_expr (te : Input.typed_expression) : Output.expression =
    let expr_type = te.annot in
    match te.expr with
    | Cst n ->
        Cst n
    | Bool b ->
        Bool b
    | Var x ->
        Var x
    | Binop (op, e1, e2) ->
        Binop (tr_op op, tr_expr e1, tr_expr e2)
    | Call (name, argv) ->
        Call (name, List.map tr_expr argv)
    | New (class_name, args) ->
        Alloc (Cst (sizeof expr_type))
    (* todo "init by constractor" *)
    | NewTab (t, size) ->
        Alloc (Binop (Mul, Cst (sizeof expr_type), tr_expr size))
    | Read mem ->
        Deref (tr_expr (tr_mem mem))
    | This ->
        Var Objlang.this_variable_name
    | MCall (obj, method_name, args) ->
        make_dcall obj method_name args
  and tr_mem (mem_access : Input.typ Input.mem) : Input.typed_expression =
    match mem_access with
    | Arr (id, index) ->
        (* address = id *)
        (* offset = index * sizeof t *)
        (* return address + offset *)
        let offset = offset_of_array id.annot index in
        {expr= Binop (Add, id, offset); annot= TInt}
    | Atr (id, field) ->
        (* address = id *)
        (* offset = offset of field *)
        (* return address + offset *)
        let id_t = id.annot in
        let classname = require_class id_t in
        let class_def = find_class classname in
        let offset = offset_of_field class_def field in
        {expr= Binop (Add, id, offset); annot= TInt}
  and make_dcall (obj : Input.typed_expression) method_name args =
    let classname = require_class obj.annot in
    let descriptor_ptr = Output.Deref (tr_expr obj) in
    let class_def = find_class classname in
    let offset = offset_of_method method_name class_def in
    let method_ptr = Output.Deref (Binop (Add, descriptor_ptr, Cst offset)) in
    DCall (method_ptr, tr_expr obj :: List.map tr_expr args)
  in
  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr : Input.typed_instruction -> Output.instruction = function
    | Putchar e ->
        Putchar (tr_expr e)
    | Set (id, expr) -> (
        let code_assignment = Output.Set (id, tr_expr expr) in
        match expr.expr with
        (* in case of creating an object *)
        | Input.New (classname, args) ->
            let descriptor_name = make_descriptor_name classname in
            let code_set_descriptor =
              Output.Write (Var id, Var descriptor_name)
            in
            let code_call_constructor =
              Output.Expr
                (make_dcall
                   (Input.mk_expr (Input.TClass classname) (Input.Var id))
                   "constructor" args )
            in
            Seq [code_assignment; code_set_descriptor; code_call_constructor]
        | other ->
            code_assignment )
    | If (cond, branch_true, branch_false) ->
        If (tr_expr cond, tr_seq branch_true, tr_seq branch_false)
    | While (cond, body) ->
        While (tr_expr cond, tr_seq body)
    | Return e ->
        Return (tr_expr e)
    | Expr e ->
        Expr (tr_expr e)
    | Write (left, right) ->
        Write (tr_expr (tr_mem left), tr_expr right)
  in
  (* translation of function definitions *)
  let tr_fdef (fdef : Input.typed_function) =
    { Imp.name= fdef.name
    ; params= List.map fst fdef.params
    ; locals= List.map fst fdef.locals
    ; code= tr_seq fdef.code }
  in
  (* class_def -> (Imp f for init descriptor, Obj f method to funtion ) *)
  let tr_class (class_def : Input.typed_class) =
    let classname = class_def.name in
    let size =
      List_ext.accumulate class_def.methods (fun e -> address_size)
      + address_size
    in
    let descriptor_name = make_descriptor_name classname in
    let descriptor_var = Output.Var descriptor_name in
    let alloc_code = Output.Set (descriptor_name, Alloc (Cst size)) in
    let set_super_code = Output.Write (descriptor_var, Cst 0) in
    let method_to_function (method_def : Input.typed_function) =
      let methodname = method_def.name in
      let new_name = Printf.sprintf "%s_%s" classname methodname in
      let new_params =
        (Input.this_variable_name, Input.TClass classname) :: method_def.params
      in
      {method_def with name= new_name; params= new_params}
    in
    let method_functions = List.map method_to_function class_def.methods in
    let descr_offset n = Output.Binop (Add, descriptor_var, Cst n) in
    let setup_methods_code =
      List.mapi
        (fun i (e : Input.typed_function) ->
          Output.Write (descr_offset ((i + 1) * address_size), Addr e.name) )
        method_functions
    in
    let (init_functions : Output.function_def) =
      { name= Printf.sprintf "init_%s_descriptor" classname
      ; params= []
      ; locals= []
      ; code= [alloc_code; set_super_code] @ setup_methods_code }
    in
    (descriptor_name, init_functions, method_functions)
  in
  let classes = p.classes in
  let class_metas = List.map tr_class classes in
  let init_functions = List.map (fun (x, y, z) -> y) class_metas in
  let method_functions =
    List.flatten (List.map (fun (x, y, z) -> z) class_metas)
  in
  let descriptor_globals = List.map (fun (x, y, z) -> x) class_metas in
  let main =
    List.find (fun (f : Input.typed_function) -> f.name = "main") p.functions
  in
  let calls_in_main =
    List.map
      (fun (f : Output.function_def) -> Output.Expr (Call (f.name, [])))
      init_functions
  in
  let new_main =
    let main' = tr_fdef main in
    {main' with code= calls_in_main @ main'.code}
  in
  let method_functions' = List.map tr_fdef method_functions in
  let without_main =
    List.filter (fun (f : Input.typed_function) -> f.name <> "main") p.functions
  in
  let old_functions = List.map tr_fdef without_main in
  let old_globals = List.map fst p.globals in
  { Imp.globals= old_globals @ descriptor_globals
  ; functions= new_main :: (method_functions' @ init_functions @ old_functions)
  }
