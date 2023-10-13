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
         directories )
  in
  (* Combine the .imp files from this directory with the ones from subdirectories *)
  sub_imp_files @ full_imp_files


let exec_name = "objlangc.exe"

let test_dir_name = "tests"

(** Execute a program in silent. All the output, stdout&stderr, of the program
    will be redirect to dev/null *)
let shell_mute program arg =
  let command = Format.sprintf "%s %s >/dev/null 2>&1" program arg in
  let return = Sys.command command in
  (program, arg, return)


let format_command program arg = Format.sprintf "%s %s" program arg

let () =
  let run_name = Sys.argv.(0) in
  let cur_dir = Filename.dirname run_name in
  let project_root = cur_dir ^ "/../" in
  let exec = Filename.concat project_root exec_name in
  let ready = Sys.file_exists exec in
  if not ready then (
    Printf.eprintf "%s does not exist" exec_name ;
    exit 0 )
  else
    let test_dir = Filename.concat project_root test_dir_name in
    let all_test_case = find_obj_files test_dir in
    if List.length all_test_case = 0 then (
      print_endline "there are not test cases" ;
      exit 0 )
    else
      let results = List.map (shell_mute exec) all_test_case in
      let failed_cases =
        List.filter (fun (_, _, return) -> return <> 0) results
      in
      if List.length failed_cases = 0 then
        print_endline "All the tests are successful!"
      else print_endline "the following test failed: " ;
      List.iter
        (fun (prog, arg, _) -> print_endline (format_command prog arg))
        failed_cases ;
      print_endline "\nPlease run each command for output."
