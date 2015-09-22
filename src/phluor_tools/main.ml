(* ============================== *)

(* Modes
- create_website

- dir

- add_brick

- copy

- compile

- clean

- list_bricks

- list_loaded_bricks
   
 *)

let (//) = Filename.concat
open Cmdliner
open Phluor_tools_lib
open Phluor_tools_lib.Settings
module F = File_operation

(* Help sections common to all commands *)

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
  Term.(pure (fun copts -> Create_website.create_website ()) $ copts_t),
  Term.info "create_website" ~sdocs:copts_sect ~doc ~man

let dir_cmd = 
  let doc = "Gives the configuration folders." in
  let man = [
    `S "DESCRIPTION";
    `P "Gives the configuration folders. You can find in it all repos, the bricks and website templates..."] @ help_secs
  in
  Term.(pure Settings.dir $ copts_t),
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
  Term.(pure F.copy $ copts_t $ src_file $ dst_file $ dico_filename $ dico_content $ prefix $ keep_dic $ inc $ keep_inc $ rem_only $ copy_inside $ avoid_error),
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
  Term.(pure Manage_bricks.add_brick_cmdl $ copts_t $ brick_name),
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
  Term.(pure Manage_bricks.remove_brick_cmdl $ copts_t $ brick_name),
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
  Term.(pure Manage_bricks.reinstall_brick_cmdl $ copts_t $ brick_name),
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
  Term.(pure Manage_bricks.update_config_brick_cmdl $ copts_t $ brick_name),
  Term.info "update_config_brick" ~sdocs:copts_sect ~doc ~man

let cd_cmd = 
  let doc = "With a dot (.) in parameters it goes in the root of the project. Else, you can choose a brick to go in." in
  let man = [
    `S "DESCRIPTION";
    `P "With a dot in parameters it goes in the root of the project. Else, you can choose a brick to go in."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (facultatif, '.' to go in the project root)." in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure Manage_bricks.cd $ copts_t $ brick_name),
  Term.info "cd" ~sdocs:copts_sect ~doc ~man

let get_loaded_bricks_cmd =
  let doc = "It gives a list of all bricks that are going to be loaded." in
  let man = [
    `S "DESCRIPTION";
    `P doc ] @ help_secs
  in
  Term.(pure Website_info.get_loaded_bricks_cmdl $ copts_t),
  Term.info "get_loaded_bricks" ~sdocs:copts_sect ~doc ~man

let compile_cmd = 
  let doc = "It compiles the whole current project. You can specify a brick name if you want to compile only one brick and its dependances." in
  let man = [
    `S "DESCRIPTION";
    `P "It compiles the whole current project."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (if not given compile everything)" in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure Website_info.compile_cmdl $ copts_t $ brick_name),
  Term.info "compile" ~sdocs:copts_sect ~doc ~man

let clean_cmd = 
  let doc = "It clean the whole current project. You can specify a brick name if you want to clean only one brick and its dependances." in
  let man = [
    `S "DESCRIPTION";
    `P "It clean the whole current project. You can specify a brick name if you want to clean only one brick and its dependances."] @ help_secs
  in
  let brick_name = 
    let doc = "Name of the brick (if not given clean everything)" in
    Arg.(value & pos 0 string "" & info [] ~docv:"BRICK" ~doc)
  in
  Term.(pure Website_info.clean_cmdl $ copts_t $ brick_name),
  Term.info "clean" ~sdocs:copts_sect ~doc ~man

let generate_www_cmd =
  let doc = "Generate the folder www/ with all the configuration of the files. Usually run it after 'compile' and becore 'run' (or use 'all' to do everything in one command)" in
  let man = [
    `S "DESCRIPTION";
    `P doc ] @ help_secs
  in
  Term.(pure Website_info.generate_www_cmdl $ copts_t),
  Term.info "generate_www" ~sdocs:copts_sect ~doc ~man

let run_cmd =
  let doc = "Run the website (use it after 'compile' and 'generate_www', or use 'all' to do everything in one command)." in
  let man = [
    `S "DESCRIPTION";
    `P doc ] @ help_secs
  in
  Term.(pure Website_info.run_cmdl $ copts_t),
  Term.info "run" ~sdocs:copts_sect ~doc ~man

let all_cmd =
  let doc = "Compile, generate the www folder and run the website." in
  let man = [
    `S "DESCRIPTION";
    `P doc ] @ help_secs
  in
  Term.(pure Website_info.all_cmdl $ copts_t),
  Term.info "all" ~sdocs:copts_sect ~doc ~man


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
       
(* Easy way to get this list :
   cat main.ml | grep -e "^let .*cmd =" | sed 's/^let //g' | sed 's/ =.*$/;/g' *)
let cmds = [
  create_website_cmd;
  dir_cmd;
  copy_cmd;
  add_brick_cmd;
  remove_brick_cmd;
  reinstall_brick_cmd;
  update_config_brick_cmd;
  cd_cmd;
  get_loaded_bricks_cmd;
  compile_cmd;
  clean_cmd;
  generate_www_cmd;
  run_cmd;
  all_cmd;
  help_cmd;
  default_cmd
]

let () = match Term.eval_choice default_cmd cmds with 
| `Error _ -> exit 1 | _ -> exit 0
