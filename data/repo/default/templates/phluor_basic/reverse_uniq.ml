(* This little script reverse a file and remove double lines at the
   end of the new file
*)

(* Using a list is usefull to reverse the file *)
let file_to_list filename =
  let ic = open_in filename in
  let rec add acc =
    try
      add ((input_line ic)::acc)
    with _ -> (close_in ic; acc)
  in
  add []

let remove_doublon out_filename file_list =
  let tbl = Hashtbl.create 10
  and oc = open_out out_filename in
  List.iter
    (fun line ->
     try
       let _ = Hashtbl.find tbl line in
       ()
     with Not_found ->
	  begin
	    Printf.fprintf oc "%s\n" line;
	    Hashtbl.add tbl line "";
	  end
    )
    file_list
		
      
let _ =
  if Array.length Sys.argv = 3 then
    let l = file_to_list Sys.argv.(1) in
    remove_doublon Sys.argv.(2) l
  else
    Printf.printf "usage: %s <in_file> <out_file>\n" Sys.argv.(0)
