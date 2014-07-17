open Printf

let () =
  (* On récupère les arguments *)
  let message = "Phluor_tools wants to provide tools \
to speed up the creation of a website based on Eliom. The available options are :"
  and copy_file_src = ref ""
  and copy_file_dst = ref ""
  and copy_dico = ref ""
  and copy_prefix = ref ""
  and copy_keep_dic = ref false
  and copy_inc = ref false
  and copy_keep_inc = ref false
  and copy_rem_only = ref false
  and copy_inside = ref false
  and avoid_error = ref false
  in
  
  let tmp_arg = ref [] in
  let args =
    [
      ("-create-project", Arg.Unit (fun () -> Phluor_create.interactive (); exit 0), " Create a new project.");
      ("-dir", Arg.Unit (fun () -> printf "%s\n" Phluor_default.data_folder; exit 0), " Display.");
      ("-add-brick",Arg.String (fun brick -> Phluor_add_brick.add_brick brick; exit 0), " Create a new project.");

      
      ("-copy-file", Arg.(Tuple [Set_string copy_file_src; Set_string copy_file_dst]), " Choose the file to copy (couple 'src dst').");
      ("-copy-dico", Arg.Set_string copy_dico, " Choose the dico file.");
      ("-copy-prefix", Arg.Set_string copy_prefix, " Choose the prefix for include files.");
      ("-copy-keep-dic", Arg.Set copy_keep_dic, " Keep the include text from dico after replacement.");
      ("-copy-inc", Arg.Set copy_inc, " Replace file inclusion.");
      ("-copy-keep-inc", Arg.Set copy_keep_inc, " Keep the include text from include after replacement.");
      ("-copy-rem-only", Arg.Set copy_rem_only, " Just remove all %%..%% text.");
      ("-copy-inside", Arg.Set copy_inside, " Copy each file inside the folder instead of the folder itself.");
      ("-avoid-error", Arg.Set avoid_error, " Avoid error if possible and continue. If not possible print the error and quit without error.");
      
      ("-h", Arg.Unit (fun x -> Printf.printf "%s" (Arg.usage_string (Arg.align !tmp_arg) message); exit 0), " Display this list of options")
    ] in
  tmp_arg := args;
  Arg.parse (Arg.align args) (fun s -> Printf.printf "The argument '%s' isn't available. Please use --help to display a help message.\n" s) message;
  try
    if !copy_file_src <> "" && !copy_file_dst <> "" then
      begin
	let f_copy = if !copy_inside then Phluor_file_operation.copy_and_replace_inside else Phluor_file_operation.copy_and_replace in
	if !copy_rem_only then
	  f_copy [] [] ~rem_inc_only:true !copy_file_src !copy_file_dst
	else begin
	    let dico = if !copy_dico = "" then [] else Phluor_file_operation.dico_of_file ~avoid_error:(!avoid_error) !copy_dico in
	    f_copy [] dico ~prefix:!copy_prefix ~keep_dic:!copy_keep_dic ~inc:!copy_inc ~keep_inc:!copy_keep_inc !copy_file_src !copy_file_dst;
	  end
      end
    else
      Printf.printf "You need to choose an option.\n%s" (Arg.usage_string (Arg.align !tmp_arg) message);

  with e -> Printf.printf "Warning : %s\n" (Printexc.to_string e)
