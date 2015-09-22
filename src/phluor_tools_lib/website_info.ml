let (//) = FilePath.concat
module F = File_operation
let sp = Printf.sprintf
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let verbose = ref true

(** Define the usual commands. The user can modify it. *)
module C = struct
  let js_of_eliom = ref "js_of_eliom"
  let ocsigenserver = ref "ocsigenserver"
end


(* Basic printf function *)
let pr x =
  Printf.ksprintf (fun s ->
      if !verbose then Printf.printf "%s" s
      else ())
    x


(* ====================================== *)
(* =====  Get the global structure  ===== *)
(* ====================================== *)

(* This dictionnary contains the list of all bricks that are supposed
   to be compiled. It contains a tupple (bool, string list)
   that says if the brick has been compiled, and gives the
   dependances of the brick. *)
module SDict = Map.Make(String)

(* When I use a "_" at the end like in fctname_ that means that the function
   is only for the current brick. Else it provide in a recursive way. *)
let get_brick_depends_ brick_name =
  let pwd = FileUtil.pwd () in
  F.(go_root `Brick);
  pr "Getting brick depends of %s...\n%!" brick_name;
  let tmp = F.list_of_file ("package" // "brick_depends.txt") in
  Sys.chdir pwd;
  tmp

let _brick_list_of_opt br_l_opt = match br_l_opt with
      None -> F.list_of_file "bricks_included.txt"
    | Some l -> l

(* Build the tree of the bricks *)
let get_tree_bricks ?brick_list () =
  let tree_bricks_dict = ref SDict.empty in
  let br_l = _brick_list_of_opt brick_list in
  let rec aux brick_list2 =
    List.iter
      (fun br ->
         let l = get_brick_depends_ br in
         if not (SDict.mem br !tree_bricks_dict) then begin
           tree_bricks_dict := SDict.add br (false, l) !tree_bricks_dict;
           aux l
         end else
           ()
      )
      brick_list2
  in aux br_l;
  tree_bricks_dict

(** Get the list of the loaded bricks in the current website *)
let get_loaded_bricks ?brick_list () =
  let tree_bricks_dict = get_tree_bricks ?brick_list () in
  SDict.fold
    (fun brick data acc -> brick :: acc)
    !tree_bricks_dict
    []

(** Get the list of the installed bricks in the current website *)
let get_installed_bricks () =
  let cwd = Sys.getcwd () in
  F.(go_root `Template);
  let tmp =
    FileUtil.(find True "." (fun x y -> y :: x) [])
    |> CCList.filter_map
      (fun s ->
         if Str.(string_match
                   (regexp "^\\./bricks_src/\\(.*\\)/root_brick$") s 0)
         then
           Some (Str.matched_group 1 s)
         else
           None) in
  Sys.chdir cwd;
  tmp


(* ===================================== *)
(* =====  Send commands to bricks  ===== *)
(* ===================================== *)

(** This function is usefull to run a command in a Lwt process *)
let _run_command' cmd_array =
  pr "Command:%s\n%!" (Array.fold_left
                         (fun a b -> sp "%s %s" a b) "" cmd_array);
  Lwt_process.exec
    (cmd_array.(0), cmd_array)

(** Send a command to a specific brick, no dependances. *)
let send_command_brick command brick_name =
  let pwd = FileUtil.pwd () in
  F.(go_root ~obj_name:brick_name `Brick);
  pr "=== Compiling %s ===\n" brick_name;
  pr "Current folder : %s\n" (FileUtil.pwd ());
  _run_command' command
  >>= (fun status ->
        match status with
        Unix.WEXITED 0 -> Lwt.return ()
      | Unix.WSIGNALED _ ->
        Lwt.fail (Failure "WSIGNALED received in compile_brick_'")
      | Unix.WSTOPPED _ ->
        Lwt.fail (Failure "WSTOPPED received in compile_brick_'")
      | Unix.WEXITED n ->
        Lwt.fail (Failure (sp "WEXITED failed with code %d'" n)))
  >>= fun () ->
  Lwt.return (Sys.chdir pwd)


(** This command is usefull to send a command to every bricks (don't forget the s). *)
let send_command_bricks command ?brick_list () =
  let br_l = _brick_list_of_opt brick_list in
  let tree_bricks_dict = get_tree_bricks ~brick_list:br_l () in
  let rec send_command_bricks_aux command brick_list =
    Lwt_list.iter_s
      (fun br ->
         let (installed, dep) =
           try
             SDict.find br !tree_bricks_dict
           with Not_found -> failwith (sp "Error in OcMake with %s" br)
         in
         (if not installed then begin
             send_command_bricks_aux command dep
             >>= fun () ->
             (send_command_brick command br
              >|= fun _ ->
              tree_bricks_dict := SDict.add br (true, dep) !tree_bricks_dict;
             )
           end else
            Lwt.return ())
         >>= fun () ->
         Lwt.return ()
      )
      brick_list
  in
  Lwt_main.run (send_command_bricks_aux command br_l)

(** Compiles all the bricks, no dependances *)
let compile_bricks ?brick_list () =
  send_command_bricks [|"make"|] ?brick_list ()

(** Compiles the current brick, no dependances *)
let clean_bricks ?brick_list () =
  send_command_bricks [|"make"; "clean"|] ?brick_list ()

(* ================================= *)
(* =====  Compilation process  ===== *)
(* ================================= *)
(* These functions are usefull to compile the website. They are
   pretty technical, a classic user shouldn't use them.*)

(** Get the list of all needed packages to link with the JS *)
let get_packages_js ?brick_list () =
  let tree_bricks_dict = get_tree_bricks ?brick_list () in
  SDict.fold
    (fun brick data acc ->
       let fn = sp "bricks_src/%s/package/lib_depends_link_js.txt" brick in
       (if FileUtil.(test Exists fn) then
          F.list_of_file fn
        else [])
       @ acc)
    !tree_bricks_dict
    []
  |> List.sort_uniq compare

(** Get the list of all .cmo that needs to be included in the server *)
let get_cmo_files ?(side="client") ?brick_list () =
  let tree_bricks_dict = get_tree_bricks ?brick_list () in
  SDict.fold
    (fun brick data acc ->
       let folder = sp "bricks_src/%s/_build/_%s/" brick side in
       (if FileUtil.(test Exists folder) then
          (* Get all *.cmo files *)
          FileUtil.ls folder
          |> List.filter (fun s -> Str.(string_match (regexp "^.*\\.cmo$") s 0))
        else [])
       @ acc)
    !tree_bricks_dict
    []

(** Compile the Js part from all .cmo files *)
let compile_js () =
  F.go_root `Template;
  FileUtil.mkdir ~parent:true "client_part/bricks";
  let js_file = "client_part/main_js.js" in
  let cmos = get_cmo_files () in
  let must_compile =
    if not FileUtil.(test Exists js_file) then true
    else
      (* Check that the cmo files are more recent than the js file *)
      cmos
      |> List.map FileUtil.(test (Is_newer_than js_file))
      |> List.fold_left (||) false
  in
  if must_compile then
    let packages =
      get_packages_js ()
      |> List.map (fun s -> [ "-package" ; s])
      |> List.flatten
    in
    Lwt_main.run
      (_run_command'
         (([
             !C.js_of_eliom;
             "-linkall";
             "-o";
             "client_part/main_js.js"]
             @ packages
             @ cmos)
          |> Array.of_list)
       >>= fun _ -> Lwt.return ())
  else
    ()

(* All these functions are used to create/update the website *)

let _save_old () =
  F.go_root `Template;
  let open FileUtil in
  (* Save the old website *)
  if test Exists "www" then begin
    pr "I'm saving the current www/ folder";
    rm ~recurse:true [".www_bak"];
    Sys.rename "www" ".www_bak";
    pr "The old configuration has been saved in the folder .www_bak/"
  end

let _create_folders () =
  F.go_root `Template;
  (* Creating the folders *)
  [
    "www/config"; (* Used to put the configuration of the website *)
    "www/local/var/log/phluor_project"; (* TODO : create a better name*)
    "www/local/var/run";
    "www/modules";  (* Will be used to store all server side bricks *)
    "www/bin_js";   (* TODO : Is it really usefull ??? *)
    "www/static/bricks"; (* This folder will contain every static file for each brick *)
  ] |> List.iter (FileUtil.mkdir ~parent:true);
  (* Link the data folder *)
  (* TODOU : Sys.ln doesn't exists... *)
  Unix.symlink ((Sys.getcwd ()) // "data") "www/local/var/data"

(* This function is used to copy the static folder of a brick. *)
let _copy_static_bricks ?brick_list () =
  F.go_root `Template;
  let tree_bricks_dict = get_tree_bricks ?brick_list () in
  SDict.iter
    (fun brick data ->
       pr "copying static %s..." brick;
       FileUtil.mkdir ~parent:true (sp "www/static/bricks/%s" brick);
       F.copy_and_replace_inside
         []
         (F.dico_of_file
            ~avoid_error:true
            (sp "bricks_src/%s/package/name.dico" brick))
         ~avoid_error:true
         (sp "bricks_src/%s/static/" brick)
         (sp "www/static/bricks/%s" brick))
    !tree_bricks_dict

let _copy_js () =
  F.go_root `Template;
  FileUtil.mkdir ~parent:true "www/static/js";
  FileUtil.cp ["client_part/main_js.js"] "www/static/js/"

let _copy_bricks_server_part () =
  F.go_root `Template;
  get_loaded_bricks ()
  |> List.iter (fun brick_name ->
      let open FileUtil in
      pr "Copying %s..." brick_name;
      mkdir ~parent:true (sp "www/modules/%s" brick_name);
      cp ~recurse:true
        (ls (sp "bricks_src/%s/_exec/" brick_name))
        (sp "www/modules/%s/" brick_name))

let _update_config () =
  let open FileUtil in
  F.go_root `Template;
  pr "--- Server configuration ---";
  (* Copy the server config... *)
  let file_conf = "www/run_server.xml" in
  cp ["run_server.xml"] file_conf;
  (* ... get the current config in the file link_server_conf.txt
     (usefull to change the configuration of the website) ... *)
  let default_dico = F.list_of_file "link_server_conf.txt"
                 |> List.filter
                   (fun s -> not Str.(string_match (regexp "^#") s 0))
                 |> List.hd
                 |> F.dico_of_file in
  (* ... and replace it with the just loaded dico *)
  F.replace_in_file [] default_dico ~avoid_error:true file_conf file_conf;
  (* Then do the same thing with the bricks *)
  get_loaded_bricks ()
  |> List.iter (fun brick_name ->
      (* If the brick is a service, use the config of another brick
         (whose name is in src/<brick>/package/new_brick_conf.txt
         TODO : Bad name ? (better would be other_brick_conf.txt)
      *)
      let brick_config =
        let content = F.list_of_file (sp "bricks_src/%s/package/new_brick_conf.txt" brick_name) in
        if content = [] || List.hd content <> "" then List.hd content
        else brick_name
      in
      pr "-- Loading the configuration of %s with the file from %s"
        brick_name brick_config;
      (* The priority for config dico is :
         - Include files of config/<brick>
         -    Else include files of bricks_src/<brick>/config
         - Include text from bricks_src/<initial brick>/package/name.dico (only BRICK_NAME)
         - Include text from config/<brick>/main.conf
         - Include text from bricks_src/<brick>/config/main.conf
      *)
      if test Exists (sp "config/%s/" brick_config) then
        F.replace_in_file [] [] ~prefix:(sp "config/%s/" brick_config)
          ~inc:true ~keep_inc:true ~avoid_error:true file_conf file_conf
      else
        F.replace_in_file [] [] ~prefix:(sp "bricks_src/%s/config" brick_config)
          ~inc:true ~keep_inc:true ~avoid_error:true file_conf file_conf;
      F.replace_in_file
        []
        ((sp "bricks_src/%s/package/name.dico" brick_name) |> F.dico_of_file)
        ~avoid_error:true file_conf file_conf;
      F.replace_in_file
        []
        ((sp "config/%s/main.conf" brick_config) |> F.dico_of_file)
        ~avoid_error:true file_conf file_conf;
      F.replace_in_file
        []
        ((sp "bricks_src/%s/config/main.conf" brick_config) |> F.dico_of_file)
        ~avoid_error:true file_conf file_conf;
    );
  (* Do again a replacement if some bricks need it *)
  F.replace_in_file [] default_dico ~avoid_error:true file_conf file_conf;
  (* Remove all the resident %%INC(...)%% *)
  F.replace_in_file [] [] ~rem_inc_only:true ~avoid_error:true
    file_conf file_conf
  

let create_and_save_website_archi () =
  F.go_root `Template;
  _save_old ();
  _create_folders ();
  _copy_js ();
  _copy_bricks_server_part ();
  _update_config ()

let run () =
  F.go_root `Template;
  pr "#################################";
  pr "########## Running... ##########";
  pr "#################################";
  (* Get port *)
  let dico = F.list_of_file "link_server_conf.txt"
             |> List.filter
               (fun s -> not Str.(string_match (regexp "^#") s 0))
             |> List.hd
             |> F.dico_of_file in
  let port =
    try F.dico_get_from_key dico "PORT"
    with Not_found -> "<Unknow>" in
  let error_message =
    try F.dico_get_from_key dico "ERROR_MESSAGE"
    with Not_found -> "" in
  pr "The website is available in port %s (for example http://localhost:%s/ )" port port;
  pr "---------------------------------";
  (* # sudo chmod 500 /etc/authbind/byport/80 *)
  (* # sudo chown leo /etc/authbind/byport/80 *)
  (* # cd www/; su --preserve-environment -c "ocsigenserver -c run_server.xml -v" *)
  pr "%s\n" error_message;
  Sys.chdir "www/";
  try
    let _ = Sys.command (F.dico_get_from_key dico "COMMAND") in
    ()
  with Not_found -> (Lwt_main.run
                       (_run_command' [|!C.ocsigenserver;
                                        "-c";
                                        "run_server.xml";
                                        "-v"|]
                        >>= fun _ -> Lwt.return ()))

(* ================================ *)
(* =====  Cmdliner functions  ===== *)
(* ================================ *)

let compile_cmdl copts brick_name =
  F.go_root `Template;
  pr "#################################";
  pr "########## BUILDING... ##########";
  pr "#################################";
  (match brick_name with
     "" ->
     (* Compile everything *)
     compile_bricks ()
   | _ ->
     (* Compile only a given brick *)
     compile_bricks ~brick_list:[brick_name] ());
  compile_js ()

let clean_cmdl copts brick_name =
    F.go_root `Template;
    match brick_name with
      "" ->
      (* Clean everything *)
      clean_bricks ()
    | _ ->
      (* Clean only a given brick, and its dependances *)
      clean_bricks ~brick_list:[brick_name] ()

let get_loaded_bricks_cmdl copts =
  F.go_root `Template;
  verbose := false;
  get_loaded_bricks ()
  |> List.iter (Printf.printf "%s ");
  Printf.printf "\n"

let generate_www_cmdl copts =
  create_and_save_website_archi ()

let run_cmdl copts =
  run ()

let all_cmdl copts =
  compile_cmdl copts "";
  generate_www_cmdl copts;
  run_cmdl copts;


(*   else if Array.length Sys.argv = 2 && Sys.argv.(1) = "list_js_packages" then *)
(*     begin *)
(*       verbose := false; *)
(*       get_packages_js brick_list *)
(*       |> List.iter (Printf.printf "-package %s "); *)
(*       Printf.printf "\n" *)
(*     end *)
(*   else if Array.length Sys.argv = 2 && Sys.argv.(1) = "get_cmo_files" then *)
(*     begin *)
(*       verbose := false; *)
(*       get_cmo_files brick_list *)
(*       |> List.iter (Printf.printf "%s "); *)
(*       Printf.printf "\n" *)
(*     end *)
(*   else if Array.length Sys.argv = 2 && Sys.argv.(1) = "build_js_file" then *)
(*     let js_file = "client_part/main_js.js" in *)
(*     let cmos = get_cmo_files brick_list in *)
(*     let must_compile = *)
(*       if not FileUtil.(test Exists js_file) then true *)
(*       else *)
(*         let time_js = FileUtil.((stat js_file).modification_time) in *)
(*         (\* Check that the cmo files are more recent than the js file *\) *)
(*         cmos *)
(*         |> List.map FileUtil.(test (Is_newer_than time_js)) *)
(*         |> List.fold_left (||) false *)
(*     in *)
(*     if must_compile then *)
(*       let packages = *)
(*         get_packages_js brick_list *)
(*         |> List.map (fun s -> [ "-package" ; s]) *)
(*         |> List.flatten *)
(*       in *)
(*       Lwt_main.run *)
(*         (_run_command' *)
(*            (([ *)
(*                C.js_of_eliom; *)
(*                "-linkall"; *)
(*                "-o"; *)
(*                "client_part/main_js.js"] *)
(*                @ packages *)
(*                @ cmos) *)
(*             |> Array.of_list)) *)
(*   else if Array.length Sys.argv = 2 && Sys.argv.(1) = "copy_static_bricks" then *)
(*       copy_static_bricks brick_list *)
(*   else *)
(*     Printf.printf "%s%!" usage *)

