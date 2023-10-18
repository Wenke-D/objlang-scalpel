type position = {line: int; column: int}

type message = string

exception SyntaxError of position

let make_position (pos : Lexing.position) =
  {line= pos.pos_lnum; column= pos.pos_cnum - pos.pos_bol}


let format_syntax_error p =
  Printf.sprintf "Syntax error at line %d, column %d" p.line p.column
