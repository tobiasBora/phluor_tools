open Batteries
module F = Phluor_file_operation
let (//) = Filename.concat


let get_repo_list () =
  (try
      FileUtil.ls (Settings.user_data_dir ("phluor" // "repo"))
    with []
  ) @
    (try
	FileUtil.ls (Phluor_default.data_folder // "repo")
      with []
    )

(** Search a brick in all folders available (first user specific, then system folders) and return it's path *)
let get_path_brick repo_list brick_name =
  let aux l = match l with
      [] -> failwith (Printf.sprintf "The brick \"%s\" isn't present in the repositories." brick_name)
    | x::r ->
       let path = x // "bricks" // brick_name in
       if FileUtil.(test Is_dir path) then
	 path
       else
	 aux r
  in aux repo_list

(* TODO : check ocaml dependencies *)
let check_dependencies repo_list brick_name =
  let check_dep_aux brick_name max_level_rec curr_rec =
    let dep_list = ref [] in
    try
      if curr_rec > max_level_rec then failwith "Too many level of recursion."
      else
	let path = get_path_brick brick_name in
	let file = path // "brick_depends.phluor" in
	if FileUtil.(test Exists file) then
	  let ic = open_in file in
	  while true do
	    let line = input_line ic in
	    dep_list := (check_dependencies line max_level_rec (curr_rec + 1))
			@ !dep_list
	  done;
    with End_of_file -> (close_in ic; !dep_list)
  in check_dep_aux brick_name 100 0
  
let add_brick repo_list brick_name =
  try
    Printf.printf "--- Checking dependencies...";
    let dep = check_dependencies repo_list brick_name in
    
    
    let path = get_path_brick brick_name in
    let dico =
      try
	F.(dico_of_question_file (path // "replacement.phluor"))
      with _ -> [] in
    F.copy_and_replace [] dico path "./"
