open Debug
open LogicError

module Input = struct
  include Objlang
end

module Output = struct
  include Imp
end

type input_expression = Input.typed_expression
type output_expression = Output.expression

let reduce = List.fold_left

let tr_op : Input.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt -> Lt

(* main translation function *)
let translate_program (p : Input.typed_program) : Output.program =
  let find_class name =
    List.find (fun (def : Input.typed_class) -> def.name = name) p.classes
  in

  (* Size of a type in bytes. *)
  let rec sizeof (t : Input.typ) =
    match t with
    | TInt -> 4
    | TBool -> 4
    | TClass s ->
        let class_def = find_class s in
        let fields = class_def.fields in
        reduce (fun sum (id, t) -> sum + sizeof t) 0 fields
    | TArray t ->
        sizeof
          t (* !!! size of a array is only the size of its element type !!! *)
    | TVoid -> 4
  in

  let offset_of_array t (i : Input.typed_expression) : Input.typed_expression =
    let (offset : Input.typed_expression) =
      { expr = Input.Cst (sizeof t); annot = TInt }
    in
    { expr = Input.Binop (Input.Mul, i, offset); annot = TInt }
  in
  let offset_of_field (class_def : Input.typed_class) field :
      Input.typed_expression =
    let rec calc fields =
      match fields with
      | (name, t) :: sub -> if name = field then 0 else sizeof t + calc sub
      | [] ->
          raise
            (ImplementationError
               (Printf.sprintf
                  "Field [%s] not found when computing offset. This should not \
                   happen as typer already checked the presence of the field"
                  field))
    in
    { expr = Cst (calc class_def.fields); annot = TInt }
  in

  (* translation of an expression *)
  let rec tr_expr (te : Input.typed_expression) : Output.expression =
    let expr_type = te.annot in
    match te.expr with
    | Cst n -> Cst n
    | Bool b -> Bool b
    | Var x -> Var x
    | Binop (op, e1, e2) -> Binop (tr_op op, tr_expr e1, tr_expr e2)
    | Call (name, argv) -> Call (name, List.map tr_expr argv)
    | New (class_name, args) -> Alloc (Cst (sizeof expr_type))
    | NewTab (t, size) ->
        Alloc (Binop (Mul, Cst (sizeof expr_type), tr_expr size))
    | Read mem -> Deref (tr_expr (tr_mem mem))
    | This -> Var "this"
    | MCall (obj, method_name, args) -> todo ""
  and tr_mem (mem_access : Input.typ Input.mem) : Input.typed_expression =
    match mem_access with
    | Arr (id, index) ->
        (* address = id *)
        (* offset = index * sizeof t *)
        (* return address + offset *)
        let offset = offset_of_array id.annot index in
        { expr = Binop (Add, id, offset); annot = TInt }
    | Atr (id, field) -> (
        (* address = id *)
        (* offset = offset of field *)
        (* return address + offset *)
        let id_t = id.annot in
        match id_t with
        | TClass name ->
            let class_def = find_class name in
            let offset = offset_of_field class_def field in
            { expr = Binop (Add, id, offset); annot = TInt }
        | _ -> raise (ImplementationError "not a class when computing offset"))
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr : Input.typed_instruction -> Imp.instruction = function
    | Putchar e -> Putchar (tr_expr e)
    | Set (id, expr) -> Set (id, tr_expr expr)
    | If (cond, branch_true, branch_false) ->
        If (tr_expr cond, tr_seq branch_true, tr_seq branch_false)
    | While (cond, body) -> While (tr_expr cond, tr_seq body)
    | Return e -> Return (tr_expr e)
    | Expr e -> Expr (tr_expr e)
    | Write (left, right) -> Write (tr_expr (tr_mem left), tr_expr right)
  in

  (* translation of function definitions *)
  let tr_fdef (fdef : Input.typed_function) =
    {
      Imp.name = fdef.name;
      params = List.map fst fdef.params;
      locals = List.map fst fdef.locals;
      code = tr_seq fdef.code;
    }
  in

  {
    Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions;
  }
