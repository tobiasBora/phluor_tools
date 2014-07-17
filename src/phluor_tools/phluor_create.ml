open Printf
type project = {mutable name: string}
let (//) = Filename.concat
  
exception Bad_answer

let letter_alphanum_reg = "^[a-zA-Z][0-9a-zA-Z_]*$"
let alphanum_reg = "^[0-9a-zA-Z_]+$"

let remove_spaces str = Str.global_replace (Str.regexp "[ \t]+") "" str
let remove_trailing_spaces str =
  Str.global_replace (Str.regexp "^[ \t]*") "" str
  |> Str.global_replace (Str.regexp "[ \t]*$") "" 
					   
(* Regexp : string of regexp * error_message : string option * string *)
let ask_one ?default ?regexp question =
  printf "%s\n   " question;
  match (read_line (), default, regexp) with
    ("", None, _) -> raise Bad_answer
  | ("", Some def, _) -> def
  | (s, _, None) -> s
  | (s, _, (Some (reg, error_msg)))
       when not (Str.string_match (Str.regexp reg) s 0)-> failwith error_msg
  | (s, _,_) -> s

let rec ask ?default ?regexp question =
  try ask_one ?default ?regexp question
  with
    Failure str -> (printf "%s\n" str; ask ?default ?regexp question)
  | _ -> (printf "You have to fill this field.\n"; ask ?default ?regexp question)
		
let interactive () =
  (* printf "%s" Phluor_default.data_folder; *)
  let dico = ref [] in
  
  (* ===== Name ===== *)
  let project_name = ask "What is the name of the project ?"
		 ~regexp:(letter_alphanum_reg, "Please use only letters, numbers and underscores, and begin with a letter.")in
  dico := [("___PROJ_NAME___", project_name)];

  (* ===== Templates ==== *)
  let templates_folder = FilePath.make_filename [Phluor_default.data_folder; "templates"] in
  let templates_list =
    FileUtil.(ls templates_folder
	      |> filter Is_dir |> List.map (FilePath.(basename))) in
  let nb_templates = List.length templates_list in

  printf "Please choose a template number.\n";
  let template_nb =
    let rec display_template n l = match l with
	[] -> ()
      | t::r -> (printf "%d - %s\n" n t; display_template (n+1) r)
    in
    let rec aux () =
      try
	display_template 1 templates_list;
	let n = read_int () in
	if n < 1 || n > nb_templates then
	  failwith "The number is not in the good range"
	else n
      with Failure e -> printf "%s\n" e; aux ()
	 | _ -> printf "You must give a number.\n"; aux ()
    in
    aux ()
  in

  let template_name = List.nth templates_list (template_nb - 1) in
  let template_folder = (FilePath.make_filename [templates_folder; template_name; "template"]) in
  printf "You chose the template %s\n" template_name;

  printf "Replacement words...\n";
  dico := !dico @
	    (Phluor_file_operation.dico_of_question_file
	       (templates_folder // template_name // "replacement.phluor"));
  
  printf "%s --> %s\n" template_folder ("./" ^ template_name);
  Phluor_file_operation.copy_and_replace_inside !dico !dico template_folder ("./" ^ project_name);
  
  printf "The project %s has been generated in %s\n" project_name project_name
