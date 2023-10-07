open Format

let () = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let c = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Asimpparser.program Asimplexer.token lb in
  close_in c;

  (* typing *)
  let tprog =
    try Asimptyper.type_program prog
    with Asimptyper.UnexpectedTypeError (expected, acutal) ->
      Asimptyper.print_unexpected_type_error expected acutal;
      exit 1
  in

  (* to imp *)
  let imp = Asimp2imp.translate_program tprog in
  (* output .imp *)
  let imp_output_file = Filename.chop_suffix file ".simp" ^ ".imp" in
  let imp_out = open_out imp_output_file in
  let imp_outf = formatter_of_out_channel imp_out in
  Imppp.print_program imp_outf imp;
  pp_print_flush imp_outf ();
  close_out imp_out;
  (* to mips *)
  let asm = Imp2mips.translate_program imp in
  (* output .asm *)
  let output_file = Filename.chop_suffix file ".simp" ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
