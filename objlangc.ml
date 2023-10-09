open Format

let () = Printexc.record_backtrace true
let source_code_file_extension = ".obj"

let () =
  let file = Sys.argv.(1) in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Objlangparser.program Objlanglexer.token lb in
  close_in c;

  (* typing *)
  let tprog =
    try Objlangtyper.type_program prog
    with TypeError.UnexpectedTypeError data ->
      print_endline (TypeError.format_unexpected_type_error data);
      exit 1
  in

  (* to imp *)
  let imp = Obj2imp.translate_program tprog in
  (* output .imp *)
  let imp_output_file =
    Filename.chop_suffix file source_code_file_extension ^ ".imp"
  in
  let imp_out = open_out imp_output_file in
  let imp_outf = formatter_of_out_channel imp_out in
  Imppp.print_program imp_outf imp;
  pp_print_flush imp_outf ();
  close_out imp_out;
  (* to mips *)
  let asm = Imp2mips.translate_program imp in
  (* output .asm *)
  let output_file =
    Filename.chop_suffix file source_code_file_extension ^ ".asm"
  in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
