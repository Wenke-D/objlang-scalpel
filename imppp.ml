open Format
open Imp

let op2string = function
  | Add -> "+"
  | Mul -> "*"
  | Lt  -> "<"

let rec print_expr fmt = function
  | Cst n -> fprintf fmt "%d" n
  | Bool b -> fprintf fmt "%b" b
  | Var x -> fprintf fmt "%s" x
  | Binop(op, e1, e2) -> 
     fprintf fmt "(@[%a %s@ %a@])" print_expr e1 (op2string op) print_expr e2
  | Call(f, args) -> fprintf fmt "%s(@[%a@])" f print_args args
  | Deref e -> fprintf fmt "*@[%a@]" print_expr e
  | Alloc e -> fprintf fmt "alloc(@[%a@])" print_expr e
and print_args fmt = function
  | [] -> fprintf fmt ""
  | [a] -> fprintf fmt "%a" print_expr a
  | a::args -> fprintf fmt "%a,@ %a" print_expr a print_args args

let rec print_instr fmt = function
  | Putchar e -> fprintf fmt "putchar(@[%a@]);" print_expr e
  | Set(x, e) -> fprintf fmt "%s = @[%a@];" x print_expr e
  | If(e, s1, s2) -> fprintf fmt "@[<v>@[<v 2>if (@[%a@]) {@,%a@]@,@[<v 2>} else {@,%a@]@,}@]" 
                       print_expr e print_seq s1 print_seq s2
  | While(e, s) -> fprintf fmt "@[<v>@[<v 2>while (@[%a@]) {@,%a@]@,}@]"
                     print_expr e print_seq s
  | Return e -> fprintf fmt "return(@[%a@]);" print_expr e
  | Expr e -> fprintf fmt "@[%a@];" print_expr e
  | Write(e1, e2) -> fprintf fmt "@[*%a =@ @[%a@];@]" print_expr e1 print_expr e2
and print_seq fmt = function
  | [] -> fprintf fmt ""
  | [i] -> fprintf fmt "%a" print_instr i
  | i::seq -> fprintf fmt "@[<v>%a@,%a@]" print_instr i print_seq seq

let rec print_params fmt = function
  | [] -> fprintf fmt ""
  | [x] -> fprintf fmt "%s" x
  | x::params -> fprintf fmt "%s,@ %a" x print_params params

let rec print_vars fmt = function
  | [] -> fprintf fmt ""
  (* | [x] -> fprintf fmt "var %s;@," x *)
  | x::vars -> fprintf fmt "@[<v>var %s;@,%a@]" x print_vars vars

let print_fdef fmt fdef =
  fprintf fmt "@[<v>@[<v 2>function %s(@[%a@]) {@,%a@,%a@]@,}@,@]" 
    fdef.name print_params fdef.params print_vars fdef.locals print_seq fdef.code

let rec print_functions fmt = function
  | [] -> fprintf fmt ""
  | fdef::functions -> fprintf fmt "@[<v>%a@,%a@]" print_fdef fdef print_functions functions

let print_program fmt p =
  fprintf fmt "@[<v>%a@,%a@]@." print_vars p.globals print_functions p.functions
