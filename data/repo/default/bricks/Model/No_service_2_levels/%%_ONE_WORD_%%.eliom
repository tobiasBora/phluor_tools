(* Here is a basic example of a brick without any global service
   (usefull for basic library). For more documentation please go on
   ROOT_PROJECT/doc/. *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open PhTools (* Provide CCOpt, PhConfig, PhDebug... *)
}}
	     
(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let brick_verb =
  PhConfig.get_value_opt dico_conf "VERBOSE"
  |> CCOpt.map int_of_string |> CCOpt.get (-1)

(* Here are some examples of configuration.
   You can remove them if you don't need them *)
let example = PhConfig.get_value dico_conf "EXAMPLE"
let silent_conf = PhConfig.get_value dico_conf "SILENT_CONF"
(* The prefix is used in service/ *)

(* Create a nice printf debug function *)
(* Ex: (here 3 is the priority of the message, see PhTools doc for
 more details) : printf 3 "Hello %s" name *)
let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt
{client{
     let printf verb fmt = PhDebug.printf %brick_name %brick_verb verb fmt
}}

(* Display the configuration, usefull to debug *)
let _ = PhConfig.print_config brick_name brick_verb

(* ======================================= *)
(* ============   Main Code   ============ *)
(* ======================================= *)
			     
(** It's really easy to use javascript : *)
{client{
     let say_hello _ =
       Eliom_lib.alert "Hello, I love your example word %s !" %example;
       printf 6 "Here is a debug printf : %s !" %example
}}

(* And server side code... *)
let give_me_five () = 5
