open Objlng
open Debug

module Env = Map.Make (String)
(** types for various environments *)

type tenv = typ Env.t
(** map<String, Simp.type> *)

type fenv = unit function_def Env.t
(** map<String, function_def<unit>> *)

type senv = struct_def Env.t
(** map<String, struct_def> *)

exception UnexpectedTypeError of typ * typ
(** (expected_type, actual_type)  *)

exception ArgumentLengthNotMatchError of int * int
(** (expected_length, actual_length)  *)

exception NotIndexableError of typ
exception NotStructureError of typ
exception UnknownFieldError of { structure : string; field : string }

let unexpected_type expect actual = raise (UnexpectedTypeError (expect, actual))

let print_unexpected_type_error expect actual =
  print_endline
    (Format.sprintf "expecting [%s], got [%s]" (string_of_typ expect)
       (string_of_typ actual))

let not_match expected actual =
  raise (ArgumentLengthNotMatchError (expected, actual))

let not_indexable t = raise (NotIndexableError t)
let not_structure t = raise (NotStructureError t)
let unknown_field s f = raise (UnknownFieldError { structure = s; field = f })

(**
    compare([te.annot], [t]) -> [te.annot]
*)
let check te t = if te.annot <> t then unexpected_type t te.annot else te

let compareAnnot e1 e2 = e1.annot = e2.annot

(** 
    [l]: List<Tuple<String, E>>;
    [env]: Map<String, E>
    [return]: Map<String, E>
    *)
let add2env l env = List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let type_program (p : unit program) : typ program =
  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty
  and fenv =
    add2env
      (List.map (fun (f : unit function_def) -> (f.name, f)) p.functions)
      Env.empty
  and senv = add2env (List.map (fun s -> (s.name, s)) p.structs) Env.empty in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env fdef.locals tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e : unit expression) : typ expression =
      match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x -> mk_expr (Env.find x tenv) (Var x)
      | Binop (op, e1, e2) ->
          let typed_e1 = type_expr e1 and typed_e2 = type_expr e2 in
          let op_t = match op with Add -> TInt | Mul -> TInt | Lt -> TBool in
          if compareAnnot typed_e1 typed_e2 then
            mk_expr op_t (Binop (op, typed_e1, typed_e2))
          else unexpected_type typed_e1.annot typed_e2.annot
      | Call (name, args) ->
          let fd = Env.find name fenv in
          let params = fd.params in
          let length_of = List.length in
          (* check arguments quantity *)
          let argc = length_of args and paramc = length_of params in
          if argc <> paramc then not_match paramc argc (* check argument type *)
          else
            (* check each argument *)
            let typed_args =
              List.map2
                (fun arg (name, param_t) ->
                  let typed_arg = type_expr arg in
                  if
                    typed_arg.annot = param_t
                    (* if match return the typed argument *)
                  then typed_arg (* else raise exception *)
                  else unexpected_type param_t typed_arg.annot)
                args params
            in
            (* typed the call by return type *)
            mk_expr fd.return (Call (name, typed_args))
      | New t -> mk_expr (TStruct t) (New t)
      | NewTab (t, size) ->
          let typed_size = type_expr size in
          (* check [size] is of type Int *)
          if typed_size.annot <> TInt then unexpected_type TInt typed_size.annot
          else mk_expr (TArray t) (NewTab (t, typed_size))
      | Read memeory ->
          let mem_t, typed_mem = type_mem memeory in
          mk_expr mem_t (Read typed_mem)
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
          | other_type -> not_indexable other_type)
      | Str (id, field) -> (
          let typed_id = type_expr id in
          (* check [id] is of type structure *)
          match typed_id.annot with
          | TStruct s -> (
              let sd = Env.find s senv in
              (* find [field] definition *)
              match List.find_opt (fun (fid, t) -> fid = field) sd.fields with
              | Some (fid, ft) -> (ft, Str (typed_id, field))
              | None -> unknown_field sd.name field)
          | other_type -> not_structure other_type)
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
  { p with functions = List.map type_fdef p.functions }
