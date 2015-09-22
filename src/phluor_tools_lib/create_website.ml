open Printf
type project = {mutable name: string}
module F = File_operation
module S = Sequence
let (//) = Filename.concat
  
exception Bad_answer

let letter_alphanum_reg = "^[a-zA-Z][0-9a-zA-Z_]*$"
let alphanum_reg = "^[0-9a-zA-Z_]+$"

let create_website ?project_name ?template_name () =
  let dico = ref [] in
  
  (* ===== Name ===== *)
  let project_name =
    match project_name with
      | None ->
        F.ask "What is the name of the project ?"
	  ~regexp:(letter_alphanum_reg,
            "Please use only letters, numbers and underscores, and begin with a letter.")
      | Some s -> s 
  in
  dico := [("___PROJ_NAME___", project_name)];

  (* ===== Templates ==== *)
  let template_name = match template_name with
      Some s -> s
    | None ->
      let template_list =
        F.(get_list_obj_repo `Template)
        |> List.map (fun s -> (s,s)) in
      F.choose_in_list template_list in
  let template_folder = F.(get_path_obj_repo `Template template_name) in
  printf "You chose the template %s\n" template_name;
  printf "Replacement words...\n";
  dico := !dico @
	    (F.dico_of_question_file
	       ~avoid_error:true
	       (template_folder // "package" // "replacement.qdico"));

  F.copy_and_replace_inside
    !dico
    !dico
    (template_folder)
    ("./" // project_name);

  printf "The project %s has copied generated in ./%s\n"
	 project_name
	 project_name;
  
  printf "-- Installing bricks...\n";
  Sys.chdir project_name;

  F.seq_of_file
    ~avoid_error:true
    (template_folder // "package" // "brick_depends.txt")
  |> S.iter (Manage_bricks.add_brick ~register:`No); (* If one wants to register a brick in the template, just edit the file in the template. *)
  
  printf "The project %s has been generated in ./%s\n"
	 project_name
	 project_name

