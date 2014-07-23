open Batteries
module F = Phluor_file_operation
module E = BatEnum
let (//) = Filename.concat

			     
(* TODO : check ocaml dependencies *)
let get_brick_dependencies brick_name =
  let rec check_dep_aux max_level_rec curr_rec brick_name =
    if curr_rec > max_level_rec then failwith "Too many level of recursion."
    else
      let path = F.(get_path_obj `Brick brick_name) in
      let file = path // "brick_depends.phluor" in
      if FileUtil.(test Exists file) then
	BatFile.lines_of file
	|> E.map (check_dep_aux max_level_rec (curr_rec + 1))
	|> E.concat
	|> E.(append (singleton brick_name))
	|> BatSet.of_enum	(* Remove doublons *)
	|> BatSet.enum
      else E.singleton brick_name
  in check_dep_aux 100 0 brick_name

(** This function doesn't mind dependencies *)
let add_one_brick brick_name =
  Printf.printf "--- Installing %s\n" brick_name;
  let path = F.(get_path_obj `Brick brick_name) in
  let dico =
      try
	F.(dico_of_question_file (path // "replacement.qdico"))
      with _ -> [] in
  F.copy_and_replace [] dico (path // "brick") "src/";
  Printf.printf "%s was installed successfully.\n" brick_name


(** This function installs dependencies too *)
let add_brick brick_name  =
  Printf.printf "--- Searching root of project...\n";
  F.go_root "root"; (* The main project must contain a root file in it's root*)
  Printf.printf "--- Checking dependencies...\n";
  get_brick_dependencies brick_name
  |> E.iter (fun x -> add_one_brick x)
