open Printf
type project = {mutable name: string}

exception Bad_answer

let alphanum_reg = "[a-zA-Z][0-9a-zA-Z_]*"

(* Regexp : string of regexp * error_message : string option * string *)
let ask_one question default regexp =
  printf "%s\n   " question;
  match (read_line (), default, regexp) with
    ("", None, _) -> raise Bad_answer
  | ("", Some def, _) -> def
  | (s, _, None) -> s
  | (s, _, (Some (reg, error_msg)))
       when not (Str.string_match (Str.regexp reg) s 0)-> failwith error_msg
  | (s, _,_) -> s

let rec ask ?default ?regexp question =
  try ask_one question default regexp
  with
    Failure str -> (printf "%s\n" str; ask ?default ?regexp question)
  | _ -> (printf "You have to fill this filed.\n"; ask ?default ?regexp question)
		
let interactive () =
  printf "%s" Phluor_default.data_folder;
  let name = ask "What is the name of the project ?"
		 ~regexp:(alphanum_reg, "Please use only letters, numbers and underscores, and begin with a letter.")in


  printf "The project %s has been generated\n" name
