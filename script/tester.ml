(** Find all obj file recursively within the give path *)
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

(** Execute a program in silent. All the output, stdout&stderr, of the program
    will be redirect to dev/null *)
let shell_mute program arg =
  let command = Format.sprintf "%s %s >/dev/null 2>&1" program arg in
  let return = Sys.command command in
  (program, arg, return)


(** Compose a shell command *)
let format_command program arg = Format.sprintf "%s %s" program arg

let hosting_dir = Filename.dirname Sys.argv.(0)

let project_root = hosting_dir ^ "/../"

let exec = Filename.concat project_root exec_name

let ready = Sys.file_exists exec

let not_empty_cases path =
  let input_dir = Filename.concat project_root path in
  let all_test_case = find_obj_files input_dir in
  if List.length all_test_case = 0 then (
    Printf.printf "there are not test cases at %s\n" input_dir ;
    exit 0 )
  else all_test_case


let positive_test_path = "tests/positive"

let run_test directory failure_filter name =
  let all_test_case = not_empty_cases directory in
  let results = List.map (shell_mute exec) all_test_case in
  let failed_cases = List.filter failure_filter results in
  if List.length failed_cases = 0 then
    Printf.printf "[%s] are all passed!\n" name
  else (
    Printf.printf "the following [%s] failed: \n" name ;
    List.iter
      (fun (prog, arg, _) -> print_endline (format_command prog arg))
      failed_cases ;
    print_endline "\nPlease run each command for output." )


let positive_test () =
  let input_dir = Filename.concat project_root positive_test_path in
  run_test input_dir (fun (_, _, return) -> return <> 0) "positive test"


let negative_test_path = "tests/negative"

let negative_test () =
  let input_dir = Filename.concat project_root negative_test_path in
  run_test input_dir (fun (_, _, return) -> return = 0) "negative test"


let () =
  if not ready then (
    Printf.eprintf "%s does not exist" exec_name ;
    exit 0 )
  else positive_test () ;
  negative_test ()
