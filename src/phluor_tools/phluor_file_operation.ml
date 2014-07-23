open Printf
let (//) = Filename.concat

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

(* TODO : use Batteries *)
let string_of_file filename =
  let ic = open_in filename in
  let str = ref "" in
  (try
      while true do
	str := !str ^ (input_line ic) ^ "\n"
      done
    with _ -> close_in ic);
  !str
			
   
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
let rec replace_include_in_string ?(sep1="%%INC(") ?(sep2=")%%") ?(prefix="") ?(keep=false) str =
  Str.global_substitute
     (Str.regexp (sep1 ^ ".+" ^ sep2))
     (fun text ->
      let inc_file = (Str.matched_string text) in
      let filename = Str.replace_first
		       (Str.regexp (sep1 ^ "\\(.+\\)" ^ sep2))
		       "\\1"
		       inc_file
      in
      (string_of_file (prefix ^ filename)) ^ (if keep then inc_file else "")
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
let dico_of_file ?(comment=true) ?(sep="=") ?(avoid_error=false) filename =
  try
    let ic = open_in filename in
    let i = ref 0 in
    let rec aux acc =
      try
	let line = input_line ic in
	incr i;
	if comment && line.[0] = '#' then aux acc
	else
	  let spl = Str.split (Str.regexp sep) line in
	  match spl with
	    [w1;w2] -> aux ((remove_trailing_spaces w1,remove_trailing_spaces w2)::acc)
	  | _ -> failwith (Printf.sprintf "An error occured on line %d : '%s'" (!i) line);
      with
	Failure s -> (close_in ic; failwith s)
      | _ -> (close_in ic; acc)
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

	   
(** Convert a question file (separate with '|', see doc for more details) in dico. Comments begins with # *)
let dico_of_question_file ?(comment=true) ?(sep="|") ?(avoid_error=false) filename =
  try
    let ic = open_in filename in
    let i = ref 0 in
    let rec aux acc =
      try
	let line = input_line ic in
	incr i;
	if comment && line.[0] = '#' then aux acc
	else
	  let spl = Str.split (Str.regexp sep) line in
	  match spl with
	    [word;question] ->
	    aux ((remove_trailing_spaces word,
		 ask (remove_trailing_spaces question))
		::acc)
	  | [word; question; default] ->
	     aux ((remove_trailing_spaces word,
		   ask ~default
		       ((remove_trailing_spaces question) ^
			  " (Default : " ^ default ^ ")"))
		  :: acc)
	  | [] -> aux acc
	  | _ -> failwith (Printf.sprintf "An error occured on line %d : '%s'" (!i) line)
      with Failure s -> (close_in ic; failwith s)
	 | End_of_file -> (close_in ic; acc)
    in
    aux []
  with Sys_error e ->
    if avoid_error then (Printf.printf "Warning : %s\n" e; [])
    else (raise (Sys_error e))

(* ==================================== *)
(* ===== File operation (copy...) ===== *)
(* ==================================== *)

	   
let replace_in_file dico_filename dico_content ?(prefix="") ?(keep_dic=false) ?(inc=false) ?(keep_inc=false) ?(rem_inc_only=false) src dst =
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
		     (if inc then replace_include_in_string ~keep:keep_inc ~prefix:prefix line1 else line1) ^ "\n" in
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

let copy_and_replace dico_filename dico_content ?prefix ?keep_dic ?inc ?keep_inc ?rem_inc_only src_dir dst_dir =
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
	src_dir
	(FilePath.(concat (make_filename [dst_dir]) (basename src_dir) ))
    else replace_in_file dico_content dico_content ?prefix ?keep_dic ?keep_inc ?inc ?rem_inc_only src_dir dst_dir

(* Same as copy_and_replace but copy the files
   which are IN the folder, not the folder itself *)
let copy_and_replace_inside dico_filename dico_content ?prefix ?keep_dic ?inc ?keep_inc ?rem_inc_only src_dir dst_dir =
  FileUtil.mkdir ~parent:true dst_dir;
  List.iter
    (fun src_file ->
     copy_and_replace dico_filename dico_content ?prefix ?keep_dic ?keep_inc ?inc ?rem_inc_only src_file dst_dir)
    FileUtil.(ls src_dir)


(* ============================== *)
(* ===== Project operations ===== *)
(* ============================== *)

(** Go in the first parent folder containing a file 'filename' *)
let go_root filename =
  let rec aux last_working_dir =
    let cwd = Sys.getcwd () in
    if last_working_dir = cwd then
      failwith "I can't find any root folder"
    else
      if FileUtil.(test Is_file filename) then ()
      else (Sys.chdir ".."; aux cwd)
  in aux ""

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

      
let get_suffix_of_obj obj_type =
  match obj_type with
    `Brick -> "bricks"
  | `Template -> "templates"
  
(** This function gives the path to an object (bricks/templates) *)
let get_path_obj obj_type name =
  let suffix = get_suffix_of_obj obj_type in
  try
    repo_list
    |> List.map (fun repo -> repo // suffix // name)
    |> List.find (FileUtil.(test Is_dir))
  with Not_found -> failwith (Printf.sprintf "The folder \"%s\" isn't present in the repositories." name)
			     
(** This function gives a list of all objects (bricks or templates) *)
let get_list_obj obj_type =
  let suffix = get_suffix_of_obj obj_type in
  repo_list
  |> List.map (fun repo -> repo // suffix)
  |> List.map (fun path -> FileUtil.(ls path
				     |> filter Is_dir
				     |> List.map
					  (FilePath.make_relative path)))
  |> List.concat

let choose_in_list l =
  let nb_el = List.length l in
  Printf.printf "Please choose an element:\n";
  let display_list l =
    List.iteri (fun i s -> printf "%d - %s\n" (i+1) s) l in
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


