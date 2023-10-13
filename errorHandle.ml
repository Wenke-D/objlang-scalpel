open Color_text

(** Compiler exit with a error message print to stderr. Exit code 1 *)
let exit_with_error text =
  Printf.printf "%s: %s\n" (danger "Error") text ;
  exit 1
