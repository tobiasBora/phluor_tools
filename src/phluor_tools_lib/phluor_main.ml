let (//) = Filename.concat
module F = File_operation

let create_website copts =
  Create_website.create_website ()

let dir copts =
  (try
      match Settings.user_data_dir ("phluor_tools") with
	Some path -> Printf.printf "Local: %s\n" path
      | None -> raise Not_found
    with _ -> Printf.printf "Local: no folder found"
  );
  Printf.printf "System: %s\n" Settings.data_folder

let copy copts src_file dst_file dico_filename dico_content prefix keep_dic inc keep_inc rem_only copy_inside avoid_error =
  let f_copy =
    if copy_inside then F.copy_and_replace_inside
    else F.copy_and_replace
  in
  if rem_only then f_copy [] [] ~rem_inc_only:true src_file dst_file
  else
    let dico_filename_final =
      if dico_filename = "" then []
      else F.dico_of_file ~avoid_error dico_filename
    in
    let dico_content_final =
      if dico_content = "" then []
      else F.dico_of_file ~avoid_error dico_content
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

