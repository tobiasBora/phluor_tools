open Batteries
module F = Phluor_file_operation
module E = BatEnum
let (//) = Filename.concat


(* TODO : find root *)
	     
let get_repo_list () =
  (try
      match Settings.user_data_dir ("phluor_tools" // "repo") with
	Some path -> FileUtil.ls path
      | None -> raise Not_found
    with _ -> []
  ) @
    (try
	FileUtil.ls (Phluor_default.data_folder // "repo")
      with _ -> []
    )

(** Search a brick in all folders available (first user specific, then system folders) and return it's path *)
let get_path_brick repo_list brick_name =
  try
    repo_list
    |> List.map (fun repo -> repo // "bricks" // brick_name)
    |> List.find (FileUtil.(test Is_dir))
  with Not_found -> failwith (Printf.sprintf "The brick \"%s\" isn't present in the repositories." brick_name)

(* TODO : check ocaml dependencies *)
let get_dependencies repo_list brick_name =
  let rec check_dep_aux brick_name max_level_rec curr_rec =
    if curr_rec > max_level_rec then failwith "Too many level of recursion."
    else
      let path = get_path_brick repo_list brick_name in
      let file = path // "brick_depends.phluor" in
      if FileUtil.(test Exists file) then
	BatFile.lines_of file
	|> E.map
	     (fun brick ->
	      check_dep_aux brick max_level_rec (curr_rec + 1)
	     )
	|> E.concat
	|> E.(append (singleton brick_name))
	|> BatSet.of_enum
	|> BatSet.enum
      else E.singleton brick_name
  in check_dep_aux brick_name 100 0

(** This function doesn't mind dependencies *)
let add_one_brick repo_list brick_name =
  Printf.printf "--- Installing %s\n" brick_name;
  let path = get_path_brick repo_list brick_name in
  let dico =
      try
	F.(dico_of_question_file (path // "replacement.phluor"))
      with _ -> [] in
  F.copy_and_replace [] dico path "src/"

(** This function installs dependencies too *)
let add_brick brick_name  =
  Printf.printf "--- Searching root of project...\n";
  F.go_root "root"; (* The main project must contain a root file in it's root*)
  Printf.printf "--- Checking dependencies...\n";
  let repo_list = get_repo_list () in
  get_dependencies repo_list brick_name
  |> E.iter (fun x -> add_one_brick repo_list x);
  Printf.printf "%s was installed successfully.\n" brick_name

		       
