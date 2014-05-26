(* replace_in_string : (string * string) list -> string *)
let rec replace_in_string dico str = match dico with
    [] -> str
  | (w1, w2)::r -> replace_in_string
		     r
		     (Str.global_replace
			(Str.regexp ("%%%" ^ w1 ^ "%%%"))
			w2
			str)

let replace_in_file dico_filename dico_content src dst =
  if FileUtil.(test Is_file (FilePath.make_filename [src])) then
    begin
      (* Replace the destination name *)
      let dst_translated = replace_in_string dico_filename dst in
      
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
	    let new_line = replace_in_string dico_content line1 ^ "\n" in
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

let copy_and_replace debug dico_filename dico_content src_dir dst_dir =
  (* Iterate on all file in folder and copy them in dst_dir *)
  List.map
    (fun file -> replace_in_file
		   dico_filename
		   dico_content
		   file
		   FilePath.(reparent
			       (make_absolute (FileUtil.pwd ()) src_dir)
			       dst_dir
			       file))
    FileUtil.(find
		True
		src_dir
		(fun x y ->
		 if FileUtil.(test Is_file y) then y :: x else x)
		[])
