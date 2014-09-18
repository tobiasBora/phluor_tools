(* This file is supposed to provide some usefull functions / information
to others bricks.
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

(* Use Kernel.Appli everytime you need to refer to the main app *)
module Appli = Eliom_registration.App (
		   struct
		     let application_name = "js/main_js"
		   end)
