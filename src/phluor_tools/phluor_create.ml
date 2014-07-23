open Printf
type project = {mutable name: string}
module F = Phluor_file_operation
let (//) = Filename.concat
  
exception Bad_answer

let letter_alphanum_reg = "^[a-zA-Z][0-9a-zA-Z_]*$"
let alphanum_reg = "^[0-9a-zA-Z_]+$"

let interactive () =
  let dico = ref [] in
  
  (* ===== Name ===== *)
  let project_name =
    F.ask "What is the name of the project ?"
	  ~regexp:(
	    letter_alphanum_reg,
	    "Please use only letters, numbers and underscores, and begin with a letter.")
  in
  dico := [("___PROJ_NAME___", project_name)];

  (* ===== Templates ==== *)
  let template_list = F.(get_list_obj `Template) in
  let template_name = F.choose_in_list template_list in
  let template_folder = F.(get_path_obj `Template template_name) in
  
  printf "You chose the template %s\n" template_name;
  printf "Replacement words...\n";
  dico := !dico @
	    (F.dico_of_question_file
	       ~avoid_error:true
	       (template_folder // "replacement.qdico"));

  F.copy_and_replace_inside
    !dico
    !dico
    (template_folder // "template")
    ("./" // project_name);

  printf "The project %s has copied generated in ./%s\n"
	 project_name
	 project_name;
  
  printf "-- Installing bricks...\n";
  Sys.chdir project_name;
  (try BatFile.lines_of (template_folder // "brick_depends.phluor")
   with _ -> BatEnum.empty ()) 
  |> BatEnum.iter Phluor_add_brick.add_brick;

  printf "The project %s has been generated in ./%s\n"
	 project_name
	 project_name
