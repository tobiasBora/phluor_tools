let (//) = Filename.concat
let name = "phluor_tools"
let data_folder = "/usr/local/share/phluor_tools/"


let user_data_dir prog_name = match Sys.os_type with
    "Unix" | "Cygwin" ->
    begin
      try Some (Sys.getenv "XDG_DATA_HOME" // prog_name)
      with Not_found ->
	try Some (Sys.getenv "HOME" // ".local" // "share" // prog_name)
	with Not_found -> None	    
    end
  | "Win32" ->
     begin
      try Some (Sys.getenv "APPDATA" // prog_name)
      with Not_found -> None
     end
  | _ -> None
	   
let user_config_dir prog_name = match Sys.os_type with
    "Unix" | "Cygwin" ->
    begin
      try Some (Sys.getenv "XDG_CONFIG_HOME" // prog_name)
      with Not_found ->
	try Some (Sys.getenv "HOME" // ".config" // prog_name)
	with Not_found -> None
    end
  | "Win32" ->
     begin
      try Some (Sys.getenv "APPDATA" // prog_name)
      with Not_found -> None	    
     end
  | _ -> None

(* ================================ *)
(* =====  Cmdliner functions  ===== *)
(* ================================ *)

(* Common options for command line *)
type copts = {debug:bool; verbose:int}

let dir copts =
  (try
      match user_data_dir ("phluor_tools") with
	  Some path -> Printf.printf "Local: %s\n" path
        | None -> raise Not_found
    with _ -> Printf.printf "Local: no folder found"
  );
  Printf.printf "System: %s\n" data_folder
