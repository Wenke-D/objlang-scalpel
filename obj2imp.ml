open Debug

type typed_expr = Asimp.typ Asimp.expression

module Input = struct
  include Asimp
end

module Output = struct
  include Imp
end

type input_expression = Input.typ Input.expression
type output_expression = Output.expression

let tr_op : Asimp.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt -> Lt

(* main translation function *)
let translate_program (p : Asimp.typ Asimp.program) =
  let find_struct_def name =
    List.find (fun (def : Asimp.struct_def) -> def.name = name) p.structs
  in

  (* Size of a type in bytes. *)
  let rec sizeof (t : Asimp.typ) =
    match t with
    | TInt -> 4
    | TBool -> 4
    | TStruct s ->
        let struct_def = find_struct_def s in
        let fields = struct_def.fields in
        List.fold_left (fun sum (id, t) -> sum + sizeof t) 0 fields
    | TArray t ->
        sizeof
          t (* !!! size of a array is only the size of its element type !!! *)
    | TVoid -> 4
  in

  let offset_of_array t (i : typed_expr) : typed_expr =
    let (offset : typed_expr) = { expr = Asimp.Cst (sizeof t); annot = TInt } in
    { expr = Asimp.Binop (Asimp.Mul, i, offset); annot = TInt }
  in
  let offset_of_struct (struct_def : Asimp.struct_def) field : typed_expr =
    let rec offset_array fields =
      match fields with
      | (name, t) :: sub ->
          if name = field then 0 else sizeof t + offset_array sub
      | [] -> failwith "field not found when computing offset"
    in
    { expr = Cst (offset_array struct_def.fields); annot = TInt }
  in

  (* translation of an expression *)
  let rec tr_expr (te : Asimp.typ Asimp.expression) : Imp.expression =
    let expr_type = te.annot in
    match te.expr with
    | Cst n -> Cst n
    | Bool b -> Bool b
    | Var x -> Var x
    | Binop (op, e1, e2) -> Binop (tr_op op, tr_expr e1, tr_expr e2)
    | Call (name, argv) -> Call (name, List.map tr_expr argv)
    | New structure -> Alloc (Cst (sizeof expr_type))
    | NewTab (t, size) ->
        Alloc (Binop (Mul, Cst (sizeof expr_type), tr_expr size))
    | Read mem -> Deref (tr_expr (tr_mem mem))
  and tr_mem (mem_access : Asimp.typ Asimp.mem) : typed_expr =
    match mem_access with
    | Arr (id, index) ->
        (* address = id *)
        (* offset = index * sizeof t *)
        (* return address + offset *)
        let offset = offset_of_array id.annot index in
        { expr = Binop (Add, id, offset); annot = TInt }
    | Str (id, field) -> (
        (* address = id *)
        (* offset = offset of field *)
        (* return address + offset *)
        let id_t = id.annot in
        match id_t with
        | TStruct name ->
            let struct_def = find_struct_def name in
            let offset = offset_of_struct struct_def field in
            { expr = Binop (Add, id, offset); annot = TInt }
        | _ -> failwith "not a structure when computing offset")
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr : Asimp.typ Asimp.instruction -> Imp.instruction = function
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
  let tr_fdef (fdef : Asimp.typ Asimp.function_def) =
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
