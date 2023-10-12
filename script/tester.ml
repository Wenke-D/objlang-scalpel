let rec find_obj_files path =
  let is_directory name = Sys.is_directory (Filename.concat path name) in
  let is_obj_file name = Filename.check_suffix name ".obj" in

  (* List all files and directories in the given path *)
  let entries = Array.to_list (Sys.readdir path) in

  (* Partition the entries into directories and files *)
  let directories, files = List.partition is_directory entries in

  (* Filter out .imp files from the files list *)
  let obj_files = List.filter is_obj_file files in

  (* Map the filenames to their full path *)
  let full_imp_files = List.map (Filename.concat path) obj_files in

  (* Recursively find .imp files in subdirectories and flatten the results *)
  let sub_imp_files =
    List.flatten
      (List.map
         (fun dir -> find_obj_files (Filename.concat path dir))
         directories)
  in

  (* Combine the .imp files from this directory with the ones from subdirectories *)
  sub_imp_files @ full_imp_files

let exec_name = "objlangc.exe"
let test_dir_name = "tests"

let exec_compiler c input =
  let pf = Printf.printf in
  let new_line = print_newline in
  let print_bar () = print_endline "----------------" in
  let print_title () = pf "compiling: %s\n" (Filename.basename input) in
  let flush () = flush stdout in
  let run () =
    let res = Sys.command (Format.sprintf "%s %s" c input) in
    new_line ();
    if res = 0 then pf "%s success\n" input else pf "%s failed\n" input
  in
  print_bar ();
  print_title ();
  new_line ();
  flush ();
  run ();
  print_bar ()

let () =
  let run_name = Sys.argv.(0) in
  let cur_dir = Filename.dirname run_name in
  let project_root = cur_dir ^ "/../" in
  let exec = Filename.concat project_root exec_name in
  let ready = Sys.file_exists exec in
  if not ready then (
    Printf.eprintf "%s does not exist" exec_name;
    exit 0)
  else
    let test_dir = Filename.concat project_root test_dir_name in
    let all_test_case = find_obj_files test_dir in
    List.iter (exec_compiler exec) all_test_case
