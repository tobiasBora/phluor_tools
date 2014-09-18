let (//) = Filename.concat

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

