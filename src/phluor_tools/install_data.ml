open Printf
(** Use : $exec dst_dir dst_filename src_dir(relative) *)
let () =
  (* === Log === *)
  let log_file = "/tmp/log_install_data" in
  FileUtil.touch log_file;
  let log_out = open_out log_file in
  let send_log msg = fprintf log_out "%s\n" msg in
  send_log "----------\nDémarrage...";
  send_log "Copying templates...\n";

  (* === Find src/dst == *)
  let dst = FilePath.make_filename [Sys.argv.(1); Sys.argv.(2)] in
  let dst_name = FilePath.(string_of_path [dst]) in

  let src = FilePath.(concat current_dir
			     (make_filename [Sys.argv.(3)])) in
  let src_name = FilePath.(string_of_path [src]) in

  send_log (sprintf "src : %s" src_name);
  send_log (sprintf "Dest : %s" dst_name);
  FileUtil.ls src
  |> List.map (fun x -> send_log (sprintf "%s" (FilePath.string_of_path [x])));

  send_log "___";

  let files_to_copy =
    FileUtil.(find
		True
		src
		(fun x y ->
		 if FileUtil.(test Is_file y) then y :: x else x) [])
  in
  files_to_copy
  |> List.map (fun x -> send_log (sprintf "%s" (FilePath.string_of_path [x])));

  send_log "___________";
  
  files_to_copy
  |> List.map (fun file -> send_log (sprintf "%s - %s - %s - %s" src dst file (FilePath.(reparent (make_absolute (FileUtil.pwd ()) src) dst file))));

  Phluor_file_operation.copy_and_replace send_log [] [] src dst;

  
  (* files_to_copy *)
  (* |> List.map (fun file -> FilePath.(reparent src dst file |> fun f -> string_of_path [f] |> send_log)); *)
 
  
  
  (* Bug with folders in FileUtil *)
(*  files_to_copy
  |> List.map (fun file -> Phluor_file_operation.replace_in_file
			  []
			  []
			  file
			  FilePath.(reparent src dst file |> fun f -> string_of_path [f])
	      ));
		*)  
  
  (* Remove, create folder and Copy *)
  (* FileUtil.rm ~recurse:true [dst]; *)
  (* FileUtil.mkdir dst; *)
  (* FileUtil.cp ~force:Force ~recurse:true (FileUtil.ls src) dst; *)
  (* FileUtil.cp ~recurse:true (FileUtil.ls src) "/tmp/dst"; *)
  (* FileUtil.(cp ~force:Force ~recurse:true files_to_copy dst); *)

  
  flush log_out;
  close_out log_out;
	   
