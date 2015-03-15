(* ============================== *)

(* Modes
- create_website

- dir

- add_brick

- copy


 *)

let (//) = Filename.concat

(* Common options *)
type copts = {debug:bool; verbose:int}

let create_website copts =
  Phluor_create_website.interactive ()

let dir copts =
  (try
      match Settings.user_data_dir ("phluor_tools") with
	Some path -> Printf.printf "Local: %s\n" path
      | None -> raise Not_found
    with _ -> Printf.printf "Local: no folder found"
  );
  Printf.printf "System: %s\n" Phluor_default.data_folder

let copy copts src_file dst_file dico_filename dico_content prefix keep_dic inc keep_inc rem_only copy_inside avoid_error =
  let f_copy =
    if copy_inside then Phluor_file_operation.copy_and_replace_inside
    else Phluor_file_operation.copy_and_replace
  in
  if rem_only then f_copy [] [] ~rem_inc_only:true src_file dst_file
  else
    let dico_filename_final =
      if dico_filename = "" then []
      else Phluor_file_operation.dico_of_file ~avoid_error dico_filename
    in
    let dico_content_final =
      if dico_content = "" then []
      else Phluor_file_operation.dico_of_file ~avoid_error dico_content
    in
    if copts.debug then
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

(* The brick_name is facultatif *)
let add_brick copts brick_name =
  (* Get the brick name *)
  let brick =
    let open Phluor_file_operation in
    let reg = Str.regexp (Printf.sprintf ".*%s.*" brick_name) in
    let l = get_list_obj `Brick in
    l
    |> List.filter (fun (br,_) ->
		    try let _ = Str.search_forward reg br 0 in true
		    with Not_found -> false)
    |> choose_in_list
  in
  Phluor_add_brick.add_brick brick

let get_brick brick_name =
  (* Deal with bricks that have been installed manually
      (not in the repository) *)
  if brick_name <> ""
     && FileUtil.(test Exists ("src/" // brick_name // "root_brick"))
  then
    brick_name
  else
    let open Phluor_file_operation in
    let reg = Str.regexp (Printf.sprintf ".*%s.*" brick_name) in
    let l = get_list_obj `Brick in
    l
    |> List.filter (fun (br,_) ->
		    try let _ = Str.search_forward reg br 0 in true
		    with Not_found -> false)
    |> choose_in_list
     
(* The brick_name is facultatif *)
let remove_brick copts brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  Phluor_file_operation.(go_root `Template);
  (* Get the brick name *)
  let brick = get_brick brick_name in
  if
    Phluor_file_operation.ask_yes_no
      ~default:"n"
      (Printf.sprintf "Are you sure you wan't to remove %s ? (y/n)" brick)
  then
    Phluor_add_brick.remove_brick brick
  else ()

(* The brick_name is facultatif *)
let reinstall_brick copts brick_name =
  (* Get the brick name *)
  let brick =
    let open Phluor_file_operation in
    let reg = Str.regexp (Printf.sprintf ".*%s.*" brick_name) in
    let l = get_list_obj `Brick in
    l
    |> List.filter (fun (br,_) ->
		    try let _ = Str.search_forward reg br 0 in true
		    with Not_found -> false)
    |> choose_in_list
  in
  Phluor_add_brick.reinstall_brick brick
  
let update_config_brick copts brick_name =
  Printf.printf "--- Searching root of project...\n";
  (* The main project must contain a root file in it's root.
     This file is useless to go to the root of the main
      website before installing a package *)
  Phluor_file_operation.(go_root `Template);
  (* Get the brick name *)
  let brick = get_brick brick_name in
  Phluor_add_brick.update_local_config brick
  
		
let help copts man_format cmds topic = match topic with
| None -> `Help (`Pager, None) (* help about the program. *)
| Some topic -> 
    let topics = "topics" :: "patterns" :: "environment" :: cmds in 
    let conv, _ = Cmdliner.Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with 
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" -> List.iter print_endline topics; `Ok ()
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok t -> 
        let page = (topic, 7, "", "", ""), [`S topic; `P "Say something";] in
        `Ok (Cmdliner.Manpage.print man_format Format.std_formatter page)

open Cmdliner

(* Help sections common to all commands *)

let copts_sect = "COMMON OPTIONS"
let help_secs = [ 
 `S copts_sect; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S "BUGS"; `P "Check bug reports at http://bugs.example.org.";]

(* Options common to all commands *)

let copts debug verbose = { debug; verbose }
let copts_t = 
  let docs = copts_sect in 
  let debug = 
    let doc = "Debug mode." in
    Arg.(value & flag & info ["debug"] ~docs ~doc)
  in
  let verbose =
    let doc = "Between 0 (no output) to 5 (say everything), configure the verbosity." in 
    Arg.(value & opt int 2 & info ["v"] ~docs ~doc)
  in 
  Term.(pure copts $ debug $ verbose)

 (* Commands *)
let create_website_cmd = 
  let doc = "Creates a new website." in
  let man = [
    `S "DESCRIPTION";
    `P "This interactive command must be used to create a new website. It will copy in a new folder a website template, and configure it."] @ help_secs
  in
  Term.(pure create_website $ copts_t),
  Term.info "create_website" ~sdocs:copts_sect ~doc ~man

let dir_cmd = 
  let doc = "Gives the configuration folders." in
  let man = [
    `S "DESCRIPTION";
    `P "Gives the configuration folders. You can find in it all repos, the bricks and website templates..."] @ help_secs
  in
  Term.(pure dir $ copts_t),
  Term.info "dir" ~sdocs:copts_sect ~doc ~man

let copy_cmd = 
  let src_file = 
    let doc = "Source file/folder to copy." in
    let none = "You need to specify a file to copy." in
    Arg.(required & pos 0 (some ~none string) None & info [] ~docv:"SRC_FILE" ~doc)
  in
  let dst_file = 
    let doc = "Destination file/folder." in
    let none = "You need to specify a destination file." in
    Arg.(required & pos 1 (some ~none string) None & info [] ~docv:"DST_FILE" ~doc)
  in
  let dico_filename =
    let doc = "Specify a dictionary (file) to use during the copy. Use it to replace filenames." in
    Arg.(value & opt string "" & info ["dico_filename"] ~docv:"DICO" ~doc)
  in
  let dico_content =
    let doc = "Specify a dictionary (file) to use during the copy. Use it to replace the text inside files." in
    Arg.(value & opt string "" & info ["dico_content"] ~docv:"DICO" ~doc)
  in
  let prefix =
    let doc = "Choose a prefix for include file (Replace %%INC(file)%% with the content of $(docv)/file). Be sure to use the flag -inc." in
    Arg.(value & opt string "" & info ["prefix"] ~docv:"PREFIX" ~doc)
  in
  let keep_dic = 
    let doc = "Don't remove the %%...%% after replacing a word from a dic." in
    Arg.(value & flag & info ["keep_dic"] ~doc)
  in
  let inc =
    let doc = "Replace the includes %%INC(...)%%" in
    Arg.(value & flag & info ["i";"inc"] ~doc)
  in
  let keep_inc = 
    let doc = "Don't remove the %%INC(...)%% after replacing this by a file." in
    Arg.(value & flag & info ["keep_inc"] ~doc)
  in
  let rem_only =
    let doc = "Remove all occurences of %%...%%" in
    Arg.(value & flag & info ["rem_only"] ~doc)
  in
  let copy_inside =
    let doc = "Copy the content of the folder instead of the folder itself." in
    Arg.(value & flag & info ["copy_inside"] ~doc)
  in
  let avoid_error =
    let doc = "Avoid raising error (for example if files don't exists...)." in
    Arg.(value & flag & info ["avoid_error"] ~doc)
  in
  let doc = "Copy some files by using includes and dicos." in
  let man = [
    `S "DESCRIPTION";
    `P "Copy files and folders by using includes and dicos. Each line in dico file must have this structure : \"WORD | REPLACEMENT\", and every occurence of %%WORD%% will be replaced by REPLACEMENT. You can use include too in your files like this : %%INC(<your file>)%%."] @ help_secs
  in
  Term.(pure copy $ copts_t $ src_file $ dst_file $ dico_filename $ dico_content $ prefix $ keep_dic $ inc $ keep_inc $ rem_only $ copy_inside $ avoid_error),
  Term.info "copy" ~sdocs:copts_sect ~doc ~man

	    
let add_brick_cmd = 
  let doc = "Add a brick to the current website." in
  let man = [
    `S "DESCRIPTION";
    `P "Add a brick to the current website. The brick name is facultatif, if you forget it the whole list of bricks will be listed."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (facultatif)." in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure add_brick $ copts_t $ brick_name),
  Term.info "add_brick" ~sdocs:copts_sect ~doc ~man

let remove_brick_cmd = 
  let doc = "Remove a brick from the current website." in
  let man = [
    `S "DESCRIPTION";
    `P "Remove a brick from the current website. The brick name is facultatif, if you forget it the whole list of bricks will be listed."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (facultatif)." in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure remove_brick $ copts_t $ brick_name),
  Term.info "remove_brick" ~sdocs:copts_sect ~doc ~man

let reinstall_brick_cmd = 
  let doc = "Reinstall a brick of the current website." in
  let man = [
    `S "DESCRIPTION";
    `P "Reinstall a brick from the current website. The brick name is facultatif, if you forget it the whole list of bricks will be listed."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (facultatif)." in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure reinstall_brick $ copts_t $ brick_name),
  Term.info "reinstall_brick" ~sdocs:copts_sect ~doc ~man

let update_config_brick_cmd = 
  let doc = "Update the brick config in config/<brick name> from the folder in src/<brick name>/config_model." in
  let man = [
    `S "DESCRIPTION";
    `P "Reinstall a brick config from the current website. The brick name is facultatif, if you forget it the whole list of bricks will be listed."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (facultatif)." in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure update_config_brick $ copts_t $ brick_name),
  Term.info "update_config_brick" ~sdocs:copts_sect ~doc ~man

	    
let help_cmd = 
  let topic = 
    let doc = "The topic to get help on. `topics' lists the topics." in 
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "Display help about phluor_tools and phluor_tools commands" in
  let man = 
    [`S "DESCRIPTION";
     `P "Prints help about phluor_tools commands."] @ help_secs
  in
  Term.(ret (pure help $ copts_t $ Term.man_format $ Term.choice_names $topic)),
  Term.info "help" ~doc ~man

let default_cmd = 
  let doc = "an ocsigen website manager." in 
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ copts_t)),
  Term.info "phluor_tools" ~version:"1.1.0" ~sdocs:copts_sect ~doc ~man
       
let cmds = [create_website_cmd; dir_cmd; copy_cmd; add_brick_cmd; remove_brick_cmd; reinstall_brick_cmd; update_config_brick_cmd; help_cmd]

let () = match Term.eval_choice default_cmd cmds with 
| `Error _ -> exit 1 | _ -> exit 0
