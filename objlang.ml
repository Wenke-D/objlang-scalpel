(**
   Annotated abstract syntax for the OBJEling language.
 *)

(* Types of SIMP values *)
type typ =
  | TInt
  | TBool
  | TClass of string (* class type, identified by its name *)
  | TArray of typ (* array containing elements of the specified type *)
  | TVoid
(* not an actual type in the source language, but having it in
   the AST makes the code more uniform *)

let rec string_of_typ t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TClass name -> "class: " ^ name
  | TArray typ -> "[" ^ string_of_typ typ ^ "]"
  | TVoid -> "void"

type binop = Add | Mul | Lt

type 'a expression = { annot : 'a; expr : 'a expr }

and 'a expr =
  | Cst of int
  | Bool of bool
  | Var of string
  | Binop of binop * 'a expression * 'a expression
  | Call of string * 'a expression list
  | MCall of 'a expression * string * 'a expression list
  | New of
      string
      * 'a expression list (* create an instance and call the constructor *)
  | NewTab of
      typ * 'a expression (* create an array of the given type and size *)
  | Read of 'a mem (* read in memory *)
  | This (* current object *)

and 'a mem =
  | Arr of 'a expression * 'a expression (* array access     e1[e2]  *)
  | Atr of 'a expression * string (* attribute access  o.x    *)

let mk_expr a e = { annot = a; expr = e }

(** 
    literal varilable name as local varilable when injecting this as a local varilable
*)
let this_variable_name = "this"

type 'a instruction =
  | Putchar of 'a expression
  | Set of string * 'a expression
  | If of 'a expression * 'a sequence * 'a sequence
  | While of 'a expression * 'a sequence
  | Return of 'a expression
  | Expr of 'a expression
  | Write of 'a mem * 'a expression (*   m = e;   *)

and 'a sequence = 'a instruction list

(* Function definition *)
type 'a function_def = {
  name : string;
  params : (string * typ) list;
  locals : (string * typ) list;
  code : 'a sequence;
  return : typ;
}

(* Class definition *)
type 'a class_def = {
  name : string;
  fields : (string * typ) list;
  methods : 'a function_def list;
  parent : string option;
}

(* Program as in IMP + types + user-defined  *)
type 'a program = {
  globals : (string * typ) list;
  functions : 'a function_def list;
  classes : 'a class_def list;
}

(* expression *)
type untyped_expression = unit expression
type typed_expression = typ expression

(* instruction *)
type untyped_instruction = unit instruction
type typed_instruction = typ instruction

(* function *)
type untyped_function = unit function_def
type typed_function = typ function_def

(* class *)
type untyped_class = unit class_def
type typed_class = typ class_def

(* program *)
type untyped_program = unit program
type typed_program = typ program
