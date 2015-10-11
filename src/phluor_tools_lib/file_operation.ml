(** This module is used to provide easy to use functions to deal with files
    and paths.
    This module depends on the module sequence. Thank you to
    Simon Cruanes for his help.
*)
let (//) = Filename.concat
module S = Sequence
let pr = Printf.printf
and sp = Printf.sprintf

(* ============================== *)
(* =====  Basic operations  ===== *)
(* ============================== *)
(* Here are the basic operations with files, for exemple
   to write in a file... *)

(** Remove the spaces in a string *)
let remove_spaces str = Str.global_replace (Str.regexp "[ \t]+") "" str

(** Remove the spaces at the beginning or at the end of a string *)
let remove_trailing_spaces str =
  Str.global_replace (Str.regexp "^[ \t]*") "" str
  |> Str.global_replace (Str.regexp "[ \t]*$") "" 

(** Check that a string isn't empty/only spaces *)
let is_not_only_spaces str =
  not (Str.string_match (Str.regexp "^[ \t\n]*$") str 0)

(** Check that a string isn't empty/only spaces or a comment *)
let is_not_only_spaces_or_comment str =
  not (Str.string_match (Str.regexp "^#") str 0)

(** Create a sequence from an input channel *)
let seq_of_ic ic = fun f -> try while true do f (input_line ic) done
  with End_of_file -> ()


(** Create a sequence from a filename. Close the file when the sequence is consumed. *)
let seq_of_file ?(avoid_error=true) filename =
  if not avoid_error || FileUtil.(test Exists filename) then
       fun f ->
        let ic = open_in filename in
        try
          while true do
            f (input_line ic)
          done
        with e -> (close_in ic; if e <> End_of_file then raise e)
  else
    S.empty

let list_of_file ?avoid_error filename =
  seq_of_file ?avoid_error filename
  |> S.to_list

(** Get a long string (containing \n) of a file *)
let string_of_file filename =
  seq_of_file filename
  |> S.fold (fun txt s -> txt ^ s ^ "\n") ""

(** Write the sequence in an output channel *)
let write_in_oc oc seq = S.iter (fun s -> output_string oc (s ^ "\n")) seq

(** Write the sequence in a file (auto close it). By default open it with right 0o664 and mode [Open_wronly; Open_creat; Open_trunc; Open_text].*)
let write_in_file ?(mode=[Open_wronly; Open_creat; Open_trunc; Open_text]) ?(perm=0o664) filename seq =
  let oc = open_out_gen mode perm filename in
  S.iter (fun s -> output_string oc (s ^ "\n")) seq;
  flush oc;
  close_out oc

let file_of_list ?mode ?perm filename list =
  write_in_file ?mode ?perm filename (S.of_list list)

let come_back path x = Sys.chdir path; x
  
let save_path f =
  let orig_path = FileUtil.pwd () in
  f ()
  |> come_back orig_path
    

exception Empty_list of string

(** Prompt a list and ask to the user to choose an item. [force_ask] can
    be used to ask for a confirmation even if their is only one element.
    [oc] is used to specify a different channel of print. It is usefull
    especilly when stdout is not usable cause phluor_tools is inside a
    $(...) (Cf. function cd) *)
let choose_in_list ?(force_ask=false) ?(oc=stdout) l =
  let nb_el = List.length l in
  if nb_el = 0 then
    raise (Empty_list "No element corresponds to the description")
  else if nb_el = 1 && not force_ask then
    fst (List.hd l)
  else begin
    Printf.fprintf oc "Please choose an element:\n";
    let display_list l =
      List.iteri (fun i s -> Printf.fprintf oc "%d - %s\n%!" (i+1) (snd s)) l in
    let rec aux () =
      try
        display_list l;
        let n = read_int () in
        if n < 1 || n > nb_el then
          failwith "The number is not in the good range"
        else n
      with Failure e -> Printf.fprintf oc "%s\n" e; aux ()
         | _ -> Printf.fprintf oc "You must give a number.\n"; aux ()
    in
    ((aux ()) - 1)
    |> List.nth l
    |> fst
  end

module Print = struct
  (** Printf title in terminal (in red) *)
  let pr_t x = ANSITerminal.(printf [Bold] x)
  (** Printf information message in terminal (in red) *)
  let pr_i x = ANSITerminal.(printf [green] x)
  (** Printf error message in terminal (in red) *)
  let pr_e x = ANSITerminal.(printf [red] x)
  (** Printf warning message in terminal (in red) *)
  let pr_w x = ANSITerminal.(printf [yellow] x)
end

open Print
    
(* =================================== *)
(* ===== File content operations ===== *)
(* =================================== *)

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
       else failwith (sp "Inc: %s doesn't exists ." (string_of_file filename))
    )
    str

(** Remove all occurences of include, usefull if you use the option keep for example*)
let remove_inc ?(sep1="%%") ?(sep2="%%") str =
  Str.global_replace
    (Str.regexp (sep1 ^ ".*" ^ sep2))
    ""
    str

(* ======================================== *)
(* ===== Generate dico from dico file ===== *)
(* ======================================== *)
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
          | _ -> failwith (sp "An error occured on line %d of %s : '%s'" (!i) filename line);
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

(** Ask a question one time, and gives the answer, a default answer, or raises an error if the given regexp isn't respected.
    Regexp : string of regexp * error_message : string option * string
    @Raises Bad_answer if the regexp isn't matched.
*)
let ask_one ?default ?regexp question =
  pr "%s\n   " question;
  match (read_line (), default, regexp) with
    ("", None, _) -> raise Bad_answer
  | ("", Some def, _) -> def
  | (s, _, None) -> s
  | (s, _, (Some (reg, error_msg)))
    when not (Str.string_match (Str.regexp reg) s 0)-> failwith error_msg
  | (s, _,_) -> s

(** Same as ask_one but ask until the regexp is matched *)
let rec ask ?default ?regexp question =
  try ask_one ?default ?regexp question
  with
    Failure str -> (pr "%s\n" str; ask ?default ?regexp question)
  | _ -> (pr "You have to fill this field.\n"; ask ?default ?regexp question)

(** Shortcut to ask a yes/no question *)
let ask_yes_no ?(default="n") question =
  match
    ask
      ~default
      ~regexp:("^[ynYN]$","Please answer y (yes) or n (no)")
      question
  with
    "y" | "Y" -> true
  | _ -> false

(** Function to pop the last element of a list *)
let rec reverse_pop l = match l with
    [] -> failwith "Empty list"
  | [x] -> ([], x)
  | x::r -> reverse_pop r |> fun (li, el) -> (x::li, el)

(** Apply one option (option is a string) to a string. It can be space_enter to convert each space into a new linve, maj to put the first letter in upper case, alphanum to keap only the alphanumeric part of a string, you also have int, float, alphabetic, alphanum-ext (alphanum + '_' and '-')... *)
let apply_one_option option str = match option with
  | "space_enter" -> Str.global_replace (Str.regexp " ") "\n" str
  | "maj" -> (let s = str in Bytes.set s 0 (Char.uppercase s.[0]); s)
  | "alphabetic" -> Str.global_replace
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
  | e -> (pr "WARNING : unknow qdico option '%s'" e; str)

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
          | _ -> failwith (sp "An error occured on line %d of %s : '%s'" (!i) filename line)
      with Failure s -> (close_in ic; failwith s)
         | End_of_file -> (close_in ic; acc)
    in
    aux []
  with Sys_error e ->
    if avoid_error then (pr "Warning : %s\n" e; [])
    else (raise (Sys_error e))

(** Maybe another structure like hastbl would make it faster. However
    it's not really time relevant so I may change this latter. *)
let rec dico_get_from_key dico key = match dico with
    [] -> raise Not_found
  | (x,c)::r when x = key -> c
  | _::r -> dico_get_from_key r key

(** Same as dico_get_from_key but gives an option instead of an error
    if needed *)
let dico_get_from_key_opt dico key =
  try
    Some (dico_get_from_key dico key)
  with Not_found -> None

(* ==================================== *)
(* ===== File operation (copy...) ===== *)
(* ==================================== *)
(** The functions here are usefull to apply operations on several files,
    replace the content in it depending on dico structures... *)

(** Get a [src] file, replace it's content depending on the options of the function, and put the result in a [dst] file. You can choose the same file for input and output.
    [replace_in_file dico_filename dico_content ?prefix ?(keep_dic=false) ?(inc=false) ?(keep_inc=false) ?(rem_inc_only=false) ?avoid_error src dst]
    [dico_filename] is a dico used to replace the filename of the file
    [dico_content] is a dico used to replace the content of the file
    [prefix] is used to replace all occurences of %%INC(<myfile>)%% with the content of <prefix>/<myfile>
    [inc] true if the %%INC(...)%% code must be replaced
    [keep_dic] if true, do not remove the %%...%% tags after replacement
    [keep_inc] same as keep_dic, but for %%INC(...)%%
    [rem_inc_only] if true, do not use any dict, just remove all %%INC(...)%% in the file
    [avoid_error] Do not raise an error if the file doesn't exists
    [src] Source file
    [dst] Destination file
*)
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

(** Same as replace_in_file, but do it also for folder if the source file is a folder (beware, the folder itself file is copied, not the content of the folder. It's like if you do [cp src_folder/ dst_folder] in Unix systems. If you want to copy the content of the folder please use copy_and_replace_inside) *)
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

(** Same as copy_and_replace but copy the files
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
  else failwith (sp "Copy: The file %s doesn't exists" src_dir)

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
     FileUtil.ls (Settings.data_folder // "repo")
   with _ -> []
  )

(* The folder repo/<obj type> *)
let get_prefix_of_obj obj_type =
  match obj_type with
    `Brick -> "bricks"
  | `Template -> "templates"

(** Each object must have in it's root content a root file whose name depends on the object. Moreover, when the brick/template is packaged an other root file must be present to be able to find it in the path *)
let get_rootfilename_of_obj obj_type =
  match obj_type with
    `Brick -> "root_brick"
  | `Template -> "root"

(** This function gives the path to an object (bricks/templates) in a repo *)
let get_path_obj_repo obj_type name =
  let prefix = get_prefix_of_obj obj_type in
  try
    repo_list
    |> List.map (fun repo -> repo // prefix // name)
    |> List.find (FileUtil.(test Is_dir))
  with Not_found -> failwith (sp "The folder \"%s\" isn't present in the repositories." name)

let get_message_file obj_type name =
  (get_path_obj_repo obj_type name) // "package" // "message.txt"

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

let rec go_root ?obj_name obj_type =
  match obj_name,obj_type with
    (None,_) -> goto_up (get_rootfilename_of_obj obj_type)
  | (Some br, `Brick) -> (go_root `Template;
                          Sys.chdir ("bricks_src" // br))
  | _ -> failwith "Go_root used with bad parameters"

let go_brick brick_name = go_root ~obj_name:brick_name `Brick

(** This function gives the asbolute path to an object (bricks/templates) in the current website *)
let get_path_obj_website ?obj_name obj_type =
  begin fun () ->
    go_root ?obj_name obj_type;
    FileUtil.pwd ()
  end |> save_path


(** This function gives a list of all objects (bricks or templates) available in the repo. If you want to get the bricks in the current website, please use [Local_website.get_installed_bricks] *)
let get_list_obj_repo obj_type =
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

(* ================================ *)
(* =====  Cmdliner functions  ===== *)
(* ================================ *)


(** This function is not really usefull. Use it only if you know what you do. *)
let copy copts src_file dst_file dico_filename dico_content prefix keep_dic inc keep_inc rem_only copy_inside avoid_error =
  let f_copy =
    if copy_inside then copy_and_replace_inside
    else copy_and_replace
  in
  if rem_only then f_copy [] [] ~rem_inc_only:true src_file dst_file
  else
    let dico_filename_final =
      if dico_filename = "" then []
      else dico_of_file ~avoid_error dico_filename
    in
    let dico_content_final =
      if dico_content = "" then []
      else dico_of_file ~avoid_error dico_content
    in
    if copts.Settings.debug then
      (
	Printf.printf "There are %d elements in the dico.\n"
		      (List.length dico_content_final);
	List.iter (fun (a,b) -> Printf.printf "(%s,%s)\n" a b)
		dico_content_final);
    f_copy
      dico_filename_final
      dico_content_final
      ~prefix
      ~keep_dic
      ~inc
      ~keep_inc
      ~avoid_error
      src_file
      dst_file
