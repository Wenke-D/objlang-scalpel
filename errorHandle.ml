(** Compiler exit with a error message print to stderr.
    Exit code 1
      *)
let exit_with_error text =
  prerr_endline text;
  exit 1
