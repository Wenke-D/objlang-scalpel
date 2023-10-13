open Format
open ErrorHandle

let () = Printexc.record_backtrace true

let source_file_extension = ".obj"

let () =
  try
    let file = Sys.argv.(1) in
    let c = open_in file in
    let lb = Lexing.from_channel c in
    let prog =
      try Objlangparser.program Objlanglexer.token lb
      with Objlangparser.Error ->
        let pos = lb.lex_curr_p in
        let (p : CompilationException.position) =
          {line= pos.pos_lnum; column= pos.pos_cnum - pos.pos_bol}
        in
        raise (CompilationException.SyntaxError p)
    in
    close_in c ;
    (* typing *)
    let tprog = Objlangtyper.type_program prog in
    (* to imp *)
    let imp = Obj2imp.translate_program tprog in
    (* output .imp *)
    let imp_output_file =
      Filename.chop_suffix file source_file_extension ^ ".imp"
    in
    let imp_out = open_out imp_output_file in
    let imp_outf = formatter_of_out_channel imp_out in
    Imppp.print_program imp_outf imp ;
    pp_print_flush imp_outf () ;
    close_out imp_out ;
    (* to mips *)
    let asm = Imp2mips.translate_program imp in
    (* output .asm *)
    let output_file =
      Filename.chop_suffix file source_file_extension ^ ".asm"
    in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mips.print_program outf asm ;
    pp_print_flush outf () ;
    close_out out ;
    exit 0
  with
  | CompilationException.SyntaxError p ->
      exit_with_error (CompilationException.format_syntax_error p)
  | CompilationException.CompilationFailure m ->
      exit_with_error (CompilationException.format_compilation_failure m)
  | TypeError.UndefinedError data ->
      exit_with_error (TypeError.format_undefined_error data)
  | TypeError.UnexpectedTypeError data ->
      exit_with_error (TypeError.format_unexpected_type_error data)
  | TypeError.ArgumentLengthError data ->
      exit_with_error (TypeError.format_argument_length_error data)
