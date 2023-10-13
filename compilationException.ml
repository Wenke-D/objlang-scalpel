type position = {line: int; column: int}

type message = string

exception SyntaxError of position

exception CompilationFailure of message

let format_syntax_error p =
  Printf.sprintf "Syntax error at line %d, column %d" p.line p.column


let format_compilation_failure m = Printf.sprintf "Compilation failure: %s" m
