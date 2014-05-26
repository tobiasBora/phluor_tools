open Printf
(** Use : $exec dst_dir dst_filename src_dir(relative) *)

let () =
  (* === Find src/dst == *)
  let src = FilePath.(concat current_dir
			     (make_filename [Sys.argv.(3)])) in
  let src_name = FilePath.(string_of_path [src]) in

  let dst = FilePath.make_filename [Sys.argv.(1); Sys.argv.(2)] in
  let dst_name = FilePath.(string_of_path [dst]) in
  
  (* === Copy === *)
  Phluor_file_operation.copy_and_replace_inside [] [] src_name dst_name;

