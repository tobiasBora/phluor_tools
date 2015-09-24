(* NB : I try to use as much as possible the library Sequence (see in
the current folder) from compagnion-cube instead of
Batteries. Sequence is faster, and I made a little module which deal
with files better than BatEnum (the BatEnum library don't close the
file if the Enum stops during enumeration) *)


module F = File_operation
module S = Sequence
let (//) = Filename.concat
let sp = Printf.sprintf
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)


(* ================================= *)
(* =====  Bricks Installation  ===== *)
(* ================================= *)

let reg_services = Str.regexp "/services$"

let is_brick_installed brick_name =
  FileUtil.(test Exists ("src/" // brick_name // "root_brick"))

let get_brick_dependencies brick_name0 =
  (* Remove /services in order to install the parent brick when
     the brick needs services. (E.g: Acme/Mybrick/services installs Acme/Mybrick *)
  let brick_name = Str.global_replace reg_services "" brick_name0 in
  (* Tries to find all dependencies of the brick *)
  let rec check_dep_aux max_level_rec curr_rec brick_name =
    if curr_rec > max_level_rec then failwith "Too many level of recursion."
    else if not (F.is_not_only_spaces brick_name) then S.empty
    else
      let path =
	try F.(get_path_obj_repo `Brick brick_name)
	with Failure e -> Printf.(printf "Error : %s" e;
			   failwith (sprintf "The brick %s cannot be found in the path." brick_name)) in
      let file = path // "package" // "brick_depends.txt" in
      if FileUtil.(test Exists file) then
	F.seq_of_file file
	|> S.map (check_dep_aux max_level_rec (curr_rec + 1))
	|> S.concat
	|> S.cons brick_name
	|> S.sort_uniq
      else S.singleton brick_name
  in check_dep_aux 100 0 brick_name


let get_ocaml_dependencies brick_seq =
  let get_depends brick_name =
    let path =
      try F.(get_path_obj_repo `Brick brick_name)
      with Failure e -> Printf.(printf "Error : %s" e;
				failwith (sprintf "The brick %s cannot be found in the path (ocaml dependencies)." brick_name)) in
    S.append
      (path // "package" // "lib_depends.txt"
       |> F.seq_of_file)
      (path // "package" // "lib_depends_link_js.txt"
       |> F.seq_of_file)
  in
  brick_seq
  |> S.filter F.is_not_only_spaces
  |> S.map get_depends
  |> S.concat
  |> S.map (fun s -> Str.split (Str.regexp "[,]") s
			   |> S.of_list)
  |> S.concat
  (*Avoid install of XXX.syntax...*)
  |> S.map (fun s -> Str.split (Str.regexp "\\.") s |> List.hd)
  |> S.filter F.is_not_only_spaces
  |> S.sort_uniq


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
let add_one_brick ?(register=`Ask) brick_name =
  (* Remove /services in order to install the parent brick when
     the brick needs services. (E.g: Acme/Mybrick/services installs Acme/Mybrick *)
  if Str.string_match (Str.regexp ".*/services$") brick_name 0 then
    ()
  else
    begin
      if F.is_not_only_spaces brick_name then
	begin
	  Printf.printf "--- Installing %s\n" brick_name;
	  let src_dir = F.(get_path_obj_repo `Brick brick_name) in
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
				  (F.seq_of_file ~avoid_error:true (src_dir // "package" // "avoid_in_copy.txt")
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
		  if not (F.seq_of_file "bricks_included.txt"
			  |> S.mem brick_name) then
		    begin
		      F.write_in_file ~mode:[Open_append]
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
    end  
  
(** This function installs dependencies too. Register can be `Ask, `Yes or `No, and is applied only for the main brick (the others are included by the first one anyway) *)
let add_brick ?register brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  Printf.printf "--- Checking dependencies...\n";
  let brick_depends = get_brick_dependencies brick_name |> S.persistent in
  brick_depends
  |> S.iter (fun br ->
	     if is_brick_installed br then
	       Printf.printf "Bricks %s already installed.\n" br
	     else if br = brick_name then
	       add_one_brick br ?register
	     else add_one_brick br ~register:`No);
  
  (* Display a short text message *)
  Printf.printf "-----------------------------\n";
  Printf.printf "---------  MESSAGE  ---------\n";
  Printf.printf "-----------------------------\n";
  (try
    F.get_message_file `Brick brick_name
    |> F.seq_of_file
    |> S.iter print_endline
  with Sys_error _ -> ());
  Printf.printf "\n";
  (* Display the command to install all ocaml depends *)
  get_ocaml_dependencies brick_depends
  |> S.fold (fun a b -> a ^ " " ^ b) ""
  |> Str.global_replace (Str.regexp "[ \t]+") " "
  |> (fun s ->
      if F.is_not_only_spaces s then
	begin
	  Printf.printf "---/ \\-------------------------\n";
	  Printf.printf "--/ | \\-----  WARNING  --------\n";
	  Printf.printf "-/__o__\\-----------------------\n";
	  Printf.printf "To conclude the installation, make sure \
			 the following libraries\n are installed, \
			 for example with :\n$ opam install %s\n"
			(F.remove_trailing_spaces s)
	end)

	      
let remove_brick ?(remove_config=true) brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  if not (is_brick_installed brick_name)
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

(** Let the user choose a brick, except if interactiv is [false] *)
let get_brick ?(local=false) ?(interactiv=true) brick_name =
  let (is_installed, available_bricks) =
    if local then
      (is_brick_installed brick_name,
       Website_info.get_installed_bricks ())
    else
      (false,
       F.get_list_obj_repo `Brick)
  in
  if brick_name <> "" && is_installed then
    brick_name
  else
    let open F in
    let reg = Str.regexp (Printf.sprintf ".*%s.*" brick_name) in
    let l = available_bricks
            |> List.filter (fun br ->
		try let _ = Str.search_forward reg br 0 in true
                with Not_found -> false)
            |> List.map (fun s -> (s,s))
    in
    if List.length l = 0 then
      raise (F.Empty_list "[Get brick] No element corresponds\
                           to the description")
    else if not interactiv then fst (List.hd l)
    else choose_in_list l


(* ======================================== *)
(* =====  Current Bricks Compilation  ===== *)
(* ======================================== *)

let get_current_brick_name () =
  let a = FileUtil.pwd () in
  F.go_root `Brick;
  let b = FileUtil.pwd () in
  let brick_name = FilePath.make_relative b a in
  Sys.chdir a;
  brick_name

(** Use this to display the content of the command *)
let run_command cmd_array =
  Printf.printf "Command:%s\n%!" (Array.fold_left
                        (fun a b -> sp "%s \"%s\"" a b) "" cmd_array);
  Lwt_process.exec
    (cmd_array.(0), cmd_array)
  >>= fun status -> match status with
    Unix.WEXITED 0 -> Lwt.return ()
  | _ -> Lwt.fail (failwith "An error has been raised during compilation...")



(* ----------------------------- *)
(* -----  Auto find files  ----- *)
(* ----------------------------- *)

(** For all "file.***" that match the regexp string "extension"
    (file must be a group), this function returns "prefix/<file>.<add_ext>".
    This function should be used only if you know what you are doing, use
    [get_server_targets] and [get_client_targets] instead.*)
let get_targets ?(folder=".") ?(avoid=["^.*OcMake_brick.ml"; "^.*myocamlbuild.ml$"]) ~extensions ?(add_ext=".cmo") prefix =
  let r_extensions = List.map Str.regexp extensions in
  let r_avoid_s = List.map Str.regexp avoid in
  let rec iter_until_true f l = match l with
      [] -> false
    | x::r -> (f x) || (iter_until_true f r)
  in
  FileUtil.ls folder
  |> CCList.filter_map
    (fun s ->
       if not (iter_until_true (fun reg -> Str.string_match reg s 0)
                 r_avoid_s)
       then
         if iter_until_true (fun reg -> Str.string_match reg s 0) r_extensions
         then
           Some (prefix // (Str.matched_group 1 s ^ add_ext))
         else
           None
       else
         None
    )

(** Return all the targets needed to generate the server part
    (basically it returns [_server/<file>.cmo] when
    [file.{eliom,ml,mlpack}] exists) *)
let get_server_targets ?avoid ?(prefix="_server") () =
  get_targets
    ?avoid
    ~extensions:["^\\(.*\\)\\.eliom$"; "^\\(.*\\)\\.ml$"; "^\\(.*\\)\\.mlpack$"]
    prefix

(** Return all the targets needed to generate the server part
    (basically it returns [_client/<file>.cmo] when
    [file.{eliom,ml,mlpack}] exists) *)
let get_client_targets ?avoid ?(prefix="_client") () =
  get_targets
    ?avoid
    ~extensions:["^\\(.*\\)\\.ml$"]
    prefix

(** Get all the libs that are present in package/lib_depends.txt *)
let get_libs () =
  let cwd = FileUtil.pwd () in
  (* Search the root of the brick *)
  F.go_root `Template;
  let list_libs = F.list_of_file "package/lib_depends.txt"
                  |> List.filter F.is_not_only_spaces in
  Sys.chdir cwd;
  list_libs

(** It loads all subdirs that are required by others bricks *)
let get_subdirs () =
  let cwd = FileUtil.pwd () in
  (* Search the root of the brick *)
  while not FileUtil.(test Exists "root_brick") do Sys.chdir ".." done;
  let tmp =
    F.list_of_file "package/brick_depends.txt"
    |> List.filter F.is_not_only_spaces
    |> List.map
      (fun s -> ["root/bricks_src/" ^ s ^ "/_build/_server";
                 "root/bricks_src/" ^ s ^ "/_build/_client"])
    |> List.flatten in
  Sys.chdir cwd;
  tmp


(* --------------------------------- *)
(* -----  Auto generate files  ----- *)
(* --------------------------------- *)

(** This function is used to automatically generate the mlpack files in
    order to pack the whole brick into one module. It take in argument
    a [(string, string) option] where the first string is the name of the
    module and the second string is the name of the folder where all the
    source files are. *)
let generate_mlpack auto_generate_mlpack = match auto_generate_mlpack with
    None -> ()
  | Some (filename, folder) ->
    let fn = filename ^ ".mlpack" in
    (* List of files in the folder with no ext *)
    let l =
      try
        get_targets
          ~folder
          ~add_ext:""
          ~extensions:["^\\(.*\\)\\.eliom$"; "^\\(.*\\)\\.ml$"; "^\\(.*\\)\\.mlpack$"]
          ""
      with Sys_error err -> failwith (sp "It may be possible that the folder '%s' doesn't exists, so that the mlpack cannot be generated. Please create it and put some .ml/.eliom files in it (Error: %s)" folder err) in
    (* This function transform "bricks_src/module" info "bricks_src/Module" *)
    let file_to_module fn =
      let i = ref (String.length fn - 1) in
      while !i > 0 && fn.[!i] <> '/' do
        decr i
      done;
      let i_final = if !i = 0 then 0 else !i + 1 in
      Bytes.set fn i_final (Char.uppercase fn.[i_final]);
      fn
    in
    let rec get_modules_str l = match l with
        [] -> ""
      | x::r -> sp "%s %s"
                  (file_to_module x)
                  (get_modules_str r)
    in
    let line_to_write = get_modules_str l in
    (* Check that the file is different *)
    let current_line =
      try
        let ic = open_in fn in
        let l = input_line ic in
        close_in ic;
        l
      with Sys_error _ -> "" in
    if current_line <> line_to_write then begin
      let oc = open_out fn in
      Printf.fprintf oc "%s\n" line_to_write;
      close_out oc;
    end


(* --------------------------------- *)
(* -----  Compile the library  ----- *)
(* --------------------------------- *)

(** This is the function that calls ocamlbuild. *)
let build_targets' ?(libs=[]) ?(subdirs=[]) ?(modules=[]) ?(preprocessor="") ?(others_options=[]) targets =
  let libs_str = String.concat "," libs in
  let subdirs_str = String.concat "," subdirs in
  let modules_str = String.concat "," modules in
  Lwt_list.iter_s
    (fun tgt ->
       run_command
         (Array.of_list
            (
              ["ocamlbuild";
               "-use-ocamlfind";
               "-plugin-tags";
               "package(eliom.ocamlbuild),package(containers),package(fileutils)"]
              @ others_options
              @ (if subdirs_str = "" then []
                 else ["-Is"; subdirs_str])
              @ (if libs_str = "" then []
                 else ["-pkgs"; libs_str])
              @ (if modules_str = "" then []
                 else ["-mods"; modules_str])
              @ (if preprocessor = "" then []
              else ["-pp"; preprocessor])
              @
              [tgt]))
       >>= fun _ ->
       Lwt.return ()
    )
    targets


let auto_build
    ?auto_generate_mlpack       (* None if the brick isn't packed,
                                   Some (pack_name, folder) else *)
    ?libs                       (* List of the libs needed
                                   (the ones in package/lib_depends.txt) *)
    ?libs_client                (* Same as libs for client
                                   (if None then libs).  *)
    ?subdirs                    (* The included subdirs (-Is ...) *)
    ?subdirs_client             (* Same as subdirs (if None then subdirs) *)
    ?modules                    (* Additionnal modules .cmo (-mods) *)
    ?modules_client             (* Same as modules for client
                                   (if None then modules) *)
    ?preprocessor               (* Preprocessor (eg: "camlp4o.opt -unsage" *)
    ?preprocessor_client        (* Same as preprocessor but for client
                                   if None then preprocessor *)
    ?others_options             (* Anything else you want to give to ocamlbuild *)
    ?others_options_client      (* Same as others options.
                                   If none then others_options *)
    ?server_targets             (* The list of server targets
                                   (like "_server/<myfile>.cmo") *)
    ?client_targets             (* The list of client targets
                                   (like "_client/<myfile>.cmo") *)
    ()
  =
  F.go_root `Brick;
  let ifNone x f = match x with None -> f () | Some el -> el in
  (* Generate the files *)
  generate_mlpack auto_generate_mlpack;
  let brick_name = get_current_brick_name () in
  (* Compile the server part *)
  Printf.printf "==== Compilation of the brick %s, server part ====\n"
    brick_name;
  let libs_no =
    ifNone
      libs
      (fun () -> F.list_of_file "package/lib_depends.txt"
                 |> List.filter F.is_not_only_spaces)
  in
  let subdirs_no =
    ifNone
      subdirs
      (fun () -> F.list_of_file "package/brick_depends.txt"
                 |> List.filter F.is_not_only_spaces
                 |> List.map
                   (fun s ->
                      ["_build/root/bricks_src/" ^ s ^ "/_build/_server";
                       "_build/root/bricks_src/" ^ s ^ "/_build/_client"])
                 |> List.flatten)
  in
  let modules_no = ifNone modules (fun () -> []) in
  let preprocessor_no = ifNone preprocessor (fun () -> "") in
  let others_options_no = ifNone others_options (fun () -> []) in
  let server_targets_no = ifNone server_targets get_server_targets in
  build_targets'
    ~libs:libs_no
    ~subdirs:subdirs_no
    ~modules:modules_no
    ~preprocessor:preprocessor_no
    ~others_options:others_options_no
    server_targets_no
  |> Lwt_main.run;
  (* Compile the client part *)
  let libs_client_no = ifNone libs_client (fun () -> libs_no) in
  let subdirs_client_no = ifNone subdirs_client (fun () -> subdirs_no) in
  let modules_client_no = ifNone modules (fun () -> modules_no) in
  let preprocessor_client_no = ifNone preprocessor (fun () -> preprocessor_no) in
  let others_options_client_no = ifNone others_options (fun () -> others_options_no) in
  let client_targets_no = ifNone client_targets get_client_targets in
  Printf.printf
    "==== Compilation of the brick %s, client part ====\n"
    brick_name;
  build_targets'
    ~libs:libs_client_no
    ~subdirs:subdirs_client_no
    ~modules:modules_client_no
    ~preprocessor:preprocessor_client_no
    ~others_options:others_options_client_no
    client_targets_no
  |> Lwt_main.run
  
(* ================================ *)
(* =====  Cmdliner functions  ===== *)
(* ================================ *)

(* The brick_name is facultatif *)
let add_brick_cmdl copts brick_name =
  get_brick brick_name
  |> add_brick 
     
(* The brick_name is facultatif *)
let remove_brick_cmdl copts brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  (* Get the brick name *)
  let brick = get_brick ~local:true brick_name in
  if
    F.ask_yes_no
      ~default:"n"
      (Printf.sprintf "Are you sure you wan't to remove %s ? (y/n)" brick)
  then
    remove_brick brick
  else ()

(* The brick_name is facultatif *)
let reinstall_brick_cmdl copts brick_name =
  get_brick brick_name
  |> reinstall_brick
  
let update_config_brick_cmdl copts brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  F.(go_root `Template);
  (* Get the brick name *)
  let brick = get_brick ~local:true brick_name in
  update_local_config brick
  
let cd copts_t brick_name = match brick_name with
    "." -> F.go_root `Template
  | _ -> get_brick ~local:true brick_name
         |> fun s -> F.go_root ~obj_name:s `Brick
