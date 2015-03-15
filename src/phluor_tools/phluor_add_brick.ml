(* NB : I try to use as much as possible the library Sequence (see in
the current folder) from compagnion-cube instead of
Batteries. Sequence is faster, and I made a little module which deal
with files better than BatEnum (the BatEnum library don't close the
file if the Enum stops during enumeration) *)


module F = Phluor_file_operation
module S = Sequence
let (//) = Filename.concat

let reg_services = Str.regexp "/services$"

			      
(* TODO : check ocaml dependencies *)
let get_brick_dependencies brick_name0 =
  (* Remove /services in order to install the parent brick when
     the brick needs services. (E.g: Acme/Mybrick/services installs Acme/Mybrick *)
  let brick_name = Str.global_replace reg_services "" brick_name0 in
  (* Tries to find all dependencies of the brick *)
  let rec check_dep_aux max_level_rec curr_rec brick_name =
    if curr_rec > max_level_rec then failwith "Too many level of recursion."
    else if Str.(string_match (regexp "^[\t ]+$") brick_name 0) then S.empty
    else
      let path =
	try F.(get_path_obj `Brick brick_name)
	with Failure e -> Printf.(printf "Error : %s" e;
			   failwith (sprintf "The brick %s cannot be found in the path." brick_name)) in
      let file = path // "package" // "brick_depends.txt" in
      if FileUtil.(test Exists file) then
	Easyfile.seq_of_file file
	|> S.map (check_dep_aux max_level_rec (curr_rec + 1))
	|> S.concat
	|> S.cons brick_name
	|> S.sort_uniq
      else S.singleton brick_name
  in check_dep_aux 100 0 brick_name

let update_local_brick_config dest dico is_model perso_folder registered_name =
  (* Generation of the folder config, from a new dico in model mode *)
  if FileUtil.(test Exists (dest // "config_model"))
  then
    begin
      let new_dico =	(* Generate the dico from qdico *)
	if not is_model then dico
	else
	  begin
	    Printf.printf "-- Configuring the new model...\n";
	    (try
		F.(dico_of_question_file
		     (dest // "package" // "replacement.qdico"))
	      with _ -> [])
	    @ (try
		  F.(dico_of_file (dest // "package" // "info.dico"))
		with _ -> [])
	  end
      in
      (* Save an eventual older configuration file *)
      if FileUtil.(test Exists perso_folder)
      then begin
	  Printf.printf "Saving the old conf...\n";
	  FileUtil.rm ~recurse:true
		      ["config" // (registered_name ^ ".bak")];
	  F.copy_and_replace
	    []
	    []
	    perso_folder
	    ("config" // (registered_name ^ ".bak"));
	  FileUtil.rm ~recurse:true [perso_folder]
	end;
      (* Copy the config_model into the user config path *)
      F.copy_and_replace_inside new_dico new_dico
				(dest // "config_model")
				perso_folder;
      Printf.printf "The configuration is now in the folder %s.\n" perso_folder
    end;
  if FileUtil.(test Exists (perso_folder // "post_install.sh"))
  then begin
      Printf.printf "Running post_install.sh...\n%!";
      let cwd = Sys.getcwd () in
      Sys.chdir perso_folder;
      let _ = Sys.command ("bash post_install.sh") in
      Sys.chdir cwd
    end

		   
(* TODO : breack this big function into smaller sub functions *)
(** This function doesn't mind dependencies *)
let add_one_brick ?(register=`Ask) brick_name0 =
  (* Remove /services in order to install the parent brick when
     the brick needs services. (E.g: Acme/Mybrick/services installs Acme/Mybrick *)
  let brick_name = Str.global_replace reg_services "" brick_name0 in
  if not (Str.string_match (Str.regexp "^[\t\n ]*$") brick_name 0) then
    begin
      Printf.printf "--- Installing %s\n" brick_name;
      let src_dir = F.(get_path_obj `Brick brick_name) in
      let dico_conf =
	try
	  F.(dico_of_file (src_dir // "package" // "info.dico"))
	with _ -> [] in
      (* dico_conf can be use as a simple dico *)
      let dico =
	F.(dico_of_question_file
	     ~avoid_error:true
	     (src_dir // "package" // "replacement.qdico"))
	@ dico_conf in
      let is_model = F.dico_get_from_key_opt dico_conf "MODE" = Some "model" in
      (* It is possible to define another brick name (useful for models) by
putting in name.dico an entry REGISTERED_NAME.
       *)
      let registered_name =
	let d = F.dico_of_file ~avoid_error:true (src_dir // "package" // "name.dico") in
	match F.dico_get_from_key_opt d "REGISTERED_NAME" with
	  None -> brick_name
	| Some tmp_name -> F.replace_in_string dico tmp_name
      in
      let dest = "src/" // registered_name in
      let perso_folder = "config" // registered_name in

      (* Avoid to copy some filenames. The "config_model" folder is copied
     after because it's content shouldn't be replaced.
       *)
      let avoid_filenames = (Str.regexp "^config_model$") ::
			      (Easyfile.seq_of_file_no_err (src_dir // "package" // "avoid_in_copy.txt")
			       |> S.map (fun s -> "^" ^ (src_dir // s) ^ "$") (* Be sure the regexp match the beginning*)
			       |> S.map (fun s -> Str.regexp s)
			       |> S.to_list)
      in
      let must_be_copied name =
	not (List.exists
	       (fun reg ->
		Str.string_match reg name 0)
	       avoid_filenames)
      in

      (* If it's a model, the package folder isn't copied, and so root_brick *)
      FileUtil.mkdir ~parent:true dest;
      FileUtil.(ls src_dir)
      |> List.filter must_be_copied
      |> List.iter
	   (fun src_file ->
	    F.copy_and_replace dico dico src_file dest);
      (* We copy the folder config_model after to avoid replacement in it *)
      if FileUtil.(test Exists (src_dir // "config_model"))
      then
	F.copy_and_replace [] [] (src_dir // "config_model") dest;

      update_local_brick_config dest dico is_model perso_folder registered_name;

      Printf.printf "%s was installed successfully.\n" brick_name;
      (* If the brick is registed in bricks_included.txt, it's possible to
     have a different register name (usefull for models). The new name
     must be in package/name.dico under REGISTERED_NAME and can refer to
     any dico reference from the above dico.
       *)
      if register <> `No then
	(
	  if (register = `Yes)
	     || F.ask_yes_no ~default:"y"
			     "\nWould you like to register the brick in bricks_included.txt\nto be load it in the website ? (y/n)"
	  then
	    begin
	      if not (Easyfile.seq_of_file "bricks_included.txt"
		      |> S.mem brick_name) then
		begin
		  Easyfile.write_in_file ~mode:[Open_append]
					 "bricks_included.txt"
					 (S.singleton registered_name);
		  Printf.printf "The brick %s has been added in bricks_included.txt\n" brick_name
		end
	      else
		Printf.printf "The brick %s is already present in bricks_included.txt\n" brick_name
	    end
	  else Printf.printf "The brick %s won't be added in bricks_included.txt\n" brick_name
	)
    end  
  
  
(** This function installs dependencies too. Register can be `Ask, `Yes or `No, and is applied only for the main brick (the others are included by the first one anyway) *)
let add_brick ?register brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  Printf.printf "--- Checking dependencies...\n";
  get_brick_dependencies brick_name
  |> S.iter (fun br ->
	     if FileUtil.(test Exists
			       ("src/" // br // "root_brick"))
	     then
	       Printf.printf "Bricks %s already installed.\n" br
	     else if br = brick_name then
	       add_one_brick br ?register
	     else add_one_brick br ~register:`No);
  
  (* Display a short text message *)
  print_endline "-----------------------------";
  try
    F.get_message_file `Brick brick_name
    |> Easyfile.seq_of_file
    |> S.iter print_endline
  with Sys_error _ -> ()
	      
let remove_brick ?(remove_config=true) brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  if not FileUtil.(test Exists
		    ("src/" // brick_name // "root_brick"))
  then
    Printf.printf "Bricks %s not installed.\n" brick_name
  else
    (
      FileUtil.rm ~recurse:true ["src/" // brick_name];
      if remove_config then
	FileUtil.rm ~recurse:true ["config/" // brick_name];
      Printf.printf "The brick %s has been successfully removed." brick_name
    )
  
let reinstall_brick brick_name =
  remove_brick ~remove_config:false brick_name;
  add_brick brick_name

let update_local_config brick_name =
  if FileUtil.(test Exists ("src/" // brick_name)) then
    let dico_conf =
      try
	F.(dico_of_file ("src/" // brick_name // "package" // "info.dico"))
      with _ -> [] in
    let dico =
      F.(dico_of_question_file
	   ~avoid_error:true
	   ("src/" // brick_name // "package" // "replacement.qdico"))
      @ dico_conf in
    update_local_brick_config
      ("src/" // brick_name)
      dico
      false
      ("config/" // brick_name)
      brick_name
  else
    Printf.printf "The brick %s doesn't exist." brick_name
