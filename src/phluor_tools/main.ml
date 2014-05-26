open Printf

let () =
  (* On récupère les arguments *)
  let message = "Phluor_tools wants to provide tools \
to speed up the creation of a website based on Eliom. The available options are :"
  in
  
  let tmp_arg = ref [] in
  let args =
    [
      ("-create_project", Arg.Unit (fun () -> Phluor_create.interactive (); exit 0), " Create a new project.");
      ("-dir", Arg.Unit (fun () -> printf "%s\n" Phluor_default.data_folder; exit 0), " Display.");
      ("-h", Arg.Unit (fun x -> Printf.printf "%s" (Arg.usage_string (Arg.align !tmp_arg) message); exit 0), " Display this list of options")
    ] in
  tmp_arg := args;
  Arg.parse (Arg.align args) (fun s -> Printf.printf "The argument '%s' isn't available. Please use --help to display a help message.\n" s) message;
  Printf.printf "You need to choose an option.\n%s" (Arg.usage_string (Arg.align !tmp_arg) message);

