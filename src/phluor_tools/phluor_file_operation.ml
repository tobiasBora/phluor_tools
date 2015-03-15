open Printf
let (//) = Filename.concat
module S = Sequence

(* ============================= *)
(* ===== General functions ===== *)
(* ============================= *)

let remove_spaces str = Str.global_replace (Str.regexp "[ \t]+") "" str
let remove_trailing_spaces str =
  Str.global_replace (Str.regexp "^[ \t]*") "" str
  |> Str.global_replace (Str.regexp "[ \t]*$") "" 

(* =================================== *)
(* ===== File content operations ===== *)
(* =================================== *)

let string_of_file filename =
  Easyfile.seq_of_file filename
  |> S.fold (fun txt s -> txt ^ s ^ "\n") ""
   
(** Replace in a string all words in the dico with it's "translation". If keep is true then keep the %%WORD%% after the replacement.
replace_in_string : (string * string) list -> string *)
let rec replace_in_string ?(sep1="%%") ?(sep2="%%") ?(keep=false) dico str =
  match dico with
    [] -> str
  | (w1, w2)::r -> replace_in_string
		     r
		     (Str.global_replace
			(Str.regexp (sep1 ^ w1 ^ sep2))
			(w2 ^ (if keep then sep1 ^ w1 ^ sep2 else ""))
			str)

		     
(** Replace all occurences of %%INC(myfile)%% with the content of prefix/myfile (the '/' should be in prefix if needed) *)
let rec replace_include_in_string ?(sep1="%%INC(") ?(sep2=")%%") ?(prefix="") ?(keep=false) ?(avoid_error=false) str =
  Str.global_substitute
     (Str.regexp (sep1 ^ ".+" ^ sep2))
     (fun text ->
      let inc_file = (Str.matched_string text) in (* With sep *)
      (* Remove the sep *)
      let filename = prefix
		     // (Str.replace_first
			   (Str.regexp (sep1 ^ "\\(.+\\)" ^ sep2))
			   "\\1"
			   inc_file)
      in
      if FileUtil.(test Exists filename) then
	(string_of_file filename) ^ (if keep then inc_file else "")
      else if avoid_error then text
      else failwith (Printf.sprintf "Inc: %s doesn't exists ." (string_of_file filename))
     )
     str

(** Remove all occurences of include, usefull if you use the option keep for example*)
let remove_inc ?(sep1="%%") ?(sep2="%%") str =
  Str.global_replace
    (Str.regexp (sep1 ^ ".*" ^ sep2))
    ""
    str

(* ========================= *)
(* ===== Generate dico ===== *)
(* ========================= *)
exception Bad_answer

(* ----------------------------------- *)
(* ----- From a simple dico file ----- *)
(* ----------------------------------- *)

(** Convert a file (separate with '=') in dico. Comments begins with # *)
let dico_of_file ?(comment=true) ?(sep_l=["=";"\\?=";"\\+=";":="]) ?(avoid_error=false) filename =
  try
    let ic = open_in filename in
    let i = ref 0 in
    let rec aux acc =
      try
	let line = input_line ic in
	incr i;
	if line = "" || (comment && line.[0] = '#') then aux acc
	else
	  let spl =
	    let rec try_sep sep_l = match sep_l with
		[] -> [line]
	      | sep::r ->
		 try
		   (* A first search is necessary to allow empty lines like
                      "VAR=" *)
		   let _ = Str.search_forward (Str.regexp sep) line 0 in
		   Str.split (Str.regexp sep) line
		 with Not_found -> try_sep r
	    in try_sep sep_l
	  in
	  match spl with
	    [w1] -> aux ((w1,"")::acc) (* Allow empty value *)
	  | [w1;w2] -> aux ((remove_trailing_spaces w1,remove_trailing_spaces w2)::acc)
	  | _ -> failwith (Printf.sprintf "An error occured on line %d of %s : '%s'" (!i) filename line);
      with
	End_of_file -> (close_in ic; acc)
      | e -> (close_in ic; raise e)
    in
    aux []
  with Sys_error e ->
    if avoid_error then []
    else (raise (Sys_error e))

(* ------------------------------------- *)
(* ----- From a question dico file ----- *)
(* ------------------------------------- *)

(* Regexp : string of regexp * error_message : string option * string *)
let ask_one ?default ?regexp question =
  Printf.printf "%s\n   " question;
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

let ask_yes_no ?(default="n") question =
  match
    ask
      ~default
      ~regexp:("^[ynYN]$","Please answer y (yes) or n (no)")
      question
  with
    "y" | "Y" -> true
    | _ -> false

	   
let rec reverse_pop l = match l with
    [] -> failwith "Empty list"
  | [x] -> ([], x)
  | x::r -> reverse_pop r |> fun (li, el) -> (x::li, el)
					       
let apply_one_option option str = match option with
  | "space_enter" -> Str.global_replace (Str.regexp " ") "\n" str
  | "maj" -> (let s = str in s.[0] <- Char.uppercase s.[0]; s)
  | "alphanum" -> Str.global_replace
			(Str.regexp "[^a-zA-Z]") "" str
  | "int" -> Str.global_replace
			(Str.regexp "[^0-9]") "" str
  | "float" -> Str.global_replace
			(Str.regexp "[^0-9.]") "" str
  | "alphanum" -> Str.global_replace
			(Str.regexp "[^a-zA-Z0-9]") "" str
  | "alphanum-ext" -> Str.global_replace
			(Str.regexp "[^a-zA-Z0-9_-]") "" str
  | "-" | "" -> str
  | e -> (Printf.printf "WARNING : unknow qdico option '%s'" e; str)
	   
(** Convert a question file (separate with '|', see doc for more details) in dico. Comments begins with # *)
let dico_of_question_file ?(comment=true) ?(sep="|") ?(avoid_error=false) filename =
  try
    let ic = open_in filename in
    let i = ref 0 in
    let rec aux acc =
      try
	let line = input_line ic in
	incr i;
	if line = "" || (comment && line.[0] = '#') then aux acc
	else
	  (* The options are always at the end, if there are two items,
             the options are facultative. *)
	  let (spl, options_str) =
	    let l = Str.split (Str.regexp sep) line in
	    if List.length l > 2 then reverse_pop l
	    else (l, "")
	  in
	  let options = Str.split (Str.regexp " ") options_str in
	  let apply_options str =
	    let rec apply_options_aux opt str = match opt with
		[] -> str
	      | x::r -> apply_options_aux r (apply_one_option x str)
	    in
	    apply_options_aux options str
	  in
	  match spl with
	    [word;question] ->
	    aux ((remove_trailing_spaces word,
		  ask (remove_trailing_spaces question) |> apply_options)
		 ::acc)
	  | [word; question; default] ->
	     aux ((remove_trailing_spaces word,
		   ask ~default
		       ((remove_trailing_spaces question) ^
			  " (Default : " ^ default ^ ")") |> apply_options)
		  :: acc)
	  | [] -> aux acc
	  | _ -> failwith (Printf.sprintf "An error occured on line %d of %s : '%s'" (!i) filename line)
      with Failure s -> (close_in ic; failwith s)
	 | End_of_file -> (close_in ic; acc)
    in
    aux []
  with Sys_error e ->
    if avoid_error then (Printf.printf "Warning : %s\n" e; [])
    else (raise (Sys_error e))

(* Maybe another structure like hastbl would make it faster. *)
let rec dico_get_from_key dico key = match dico with
    [] -> raise Not_found
  | (x,c)::r when x = key -> c
  | _::r -> dico_get_from_key r key

let dico_get_from_key_opt dico key =
  try
    Some (dico_get_from_key dico key)
  with Not_found -> None

(* ==================================== *)
(* ===== File operation (copy...) ===== *)
(* ==================================== *)

	   
let replace_in_file dico_filename dico_content ?prefix ?(keep_dic=false) ?(inc=false) ?(keep_inc=false) ?(rem_inc_only=false) ?avoid_error src dst =
  (* Replace the destination name *)
  let dst_translated = replace_in_string dico_filename dst in
     
  if FileUtil.(test Is_file (FilePath.make_filename [src])) then
    begin
      (* Check if file are the same, if it's the case use a temp file *)
      let same_file = FilePath.(reduce (make_filename [src])
				= reduce (make_filename [dst_translated])) in
      let new_dst = if same_file then Filename.temp_file "Phluor_" "_copy"
		    else dst_translated in

      let is_exec = FileUtil.(test Is_exec src) in
      
      (* Create all folders of new_dst *)
      FileUtil.mkdir
	~parent:true
	(FilePath.(dirname (make_filename [dst_translated])));
  
      (* Open channels *)
      let src_ic = open_in src
      and dst_oc = open_out new_dst in
      (* Copy content *)
      (try
	  while true do
	    let line1 = input_line src_ic in
	    let new_line =
	      if rem_inc_only then
		(remove_inc line1) ^ "\n"
	      else replace_in_string
		     dico_content
		     ~keep:keep_dic
		     (if inc then replace_include_in_string ~keep:keep_inc ?prefix ?avoid_error line1 else line1) ^ "\n" in
	    output_string dst_oc new_line
	  done
	with End_of_file -> (flush dst_oc; close_in src_ic; close_out dst_oc));

      (* If temp file, copy the new content *)
      if same_file then
	FileUtil.cp ~recurse:true [new_dst] dst_translated;
      
      if is_exec then
	Unix.(chmod (* Unix because FileUtil doesn't provide it yet *)
		new_dst
		( Unix.((lstat new_dst).st_perm) + 0o111))
    end
  else if FileUtil.(test Is_dir (FilePath.make_filename [src])) then
    FileUtil.mkdir ~parent:true dst_translated

let copy_and_replace dico_filename dico_content ?prefix ?keep_dic ?inc ?keep_inc ?rem_inc_only ?avoid_error src_dir dst_dir =
  (* Iterate on all file in folder and copy them in dst_dir *)
  if FileUtil.(test Is_dir src_dir) then
    List.iter
      (fun file -> replace_in_file
		     dico_filename
		     dico_content
		     ?prefix
		     ?keep_dic
		     ?keep_inc
		     ?inc
		     ?rem_inc_only
		     ?avoid_error
		     file
		     FilePath.(reparent
				 (make_absolute (FileUtil.pwd ()) (dirname src_dir))
				 (make_absolute (FileUtil.pwd ()) dst_dir)
				 (make_absolute (FileUtil.pwd ()) file)))
      FileUtil.(find
		  True
		  src_dir
		  (fun x y ->
		   if FileUtil.(test (Or (Is_file,Is_dir)) y) then y :: x else x)
		  [])
  else if FileUtil.(test Is_file src_dir) then
    if FileUtil.(test Is_dir dst_dir) then
      replace_in_file
	dico_filename
	dico_content
	?prefix
	?keep_dic
	?keep_inc
	?inc
	?rem_inc_only
	?avoid_error
	src_dir
	(FilePath.(concat (make_filename [dst_dir]) (basename src_dir) ))
    else replace_in_file dico_content dico_content ?prefix ?keep_dic ?keep_inc ?inc ?rem_inc_only ?avoid_error src_dir dst_dir

(* Same as copy_and_replace but copy the files
   which are IN the folder, not the folder itself *)
let copy_and_replace_inside dico_filename dico_content ?prefix ?keep_dic ?inc ?keep_inc ?rem_inc_only ?avoid_error src_dir dst_dir =
  if FileUtil.(test Exists src_dir)
  then begin
      FileUtil.mkdir ~parent:true dst_dir;
      List.iter
	(fun src_file ->
	 copy_and_replace dico_filename dico_content ?prefix ?keep_dic ?keep_inc ?inc ?rem_inc_only ?avoid_error src_file dst_dir)
	FileUtil.(ls src_dir)
    end
  else if avoid_error = None || avoid_error = Some true then ()
  else failwith (Printf.sprintf "Copy: The file %s doesn't exists" src_dir)

(* ============================ *)
(* ===== Path operations  ===== *)
(* ============================ *)
	 
(** Each template/brick is in a repositories. This function gives the list of repositories, the user specific folder in first position *)
let repo_list =
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

(* The folder repo/<obj type> *)
let get_prefix_of_obj obj_type =
  match obj_type with
    `Brick -> "bricks"
  | `Template -> "templates"

(* NON SENSE (\* The folder repo/objtype/name/<content> *\) *)
(* let get_prefix_of_content obj_type = *)
(*   match obj_type with *)
(*     `Brick -> "brick" *)
(*   | `Template -> "template" *)

(* Each object must have in it's root content a root file whose name depends on the object. Moreover, when the brick/template is packaged an other root file must be present to be able to find it in the path *)
let get_rootfilename_of_obj obj_type =
  match obj_type with
    `Brick -> "root_brick"
  | `Template -> "root"

(** This function gives the path to an object (bricks/templates) in a repo *)
let get_path_obj obj_type name =
  let prefix = get_prefix_of_obj obj_type in
  try
    repo_list
    |> List.map (fun repo -> repo // prefix // name)
    |> List.find (FileUtil.(test Is_dir))
  with Not_found -> failwith (Printf.sprintf "The folder \"%s\" isn't present in the repositories." name)

let get_message_file obj_type name =
  (get_path_obj obj_type name) // "package" // "message.txt"

(** Go in the first parent folder containing a file 'filename' *)
let goto_up filename =
  let rec aux last_working_dir =
    let cwd = Sys.getcwd () in
    if last_working_dir = cwd then
      failwith "I can't find any root folder"
    else
      if FileUtil.(test Is_file filename) then ()
      else (Sys.chdir ".."; aux cwd)
  in aux ""

let go_root obj_type = goto_up (get_rootfilename_of_obj obj_type) 
			     
(** This function gives a list of all objects (bricks or templates) available in the repo *)
let get_list_obj obj_type =
  let prefix = get_prefix_of_obj obj_type in
  let rootf = get_rootfilename_of_obj obj_type in
  repo_list
  |> List.map (fun repo -> repo // prefix)
  |> List.map (fun path -> FileUtil.(find True path (fun x y -> y :: x) []
				     |> filter Is_dir
				     |> List.filter
					  (fun f ->
					   test
					     Exists
					     (f // rootf))
				     |> List.map
					  (FilePath.make_relative path)))
  |> List.concat
  |> List.map (fun s -> (s,s))	(* The first is name, the second is used for display *)

let choose_in_list l =
  let nb_el = List.length l in
  Printf.printf "Please choose an element:\n";
  let display_list l =
    List.iteri (fun i s -> printf "%d - %s\n" (i+1) (snd s)) l in
  let rec aux () =
    try
      display_list l;
      let n = read_int () in
      if n < 1 || n > nb_el then
	failwith "The number is not in the good range"
      else n
    with Failure e -> printf "%s\n" e; aux ()
       | _ -> printf "You must give a number.\n"; aux ()
  in
  ((aux ()) - 1)
  |> List.nth l
  |> fst


