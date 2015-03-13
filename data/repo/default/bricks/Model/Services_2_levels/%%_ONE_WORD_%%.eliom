(* Here is a basic example of a brick using services. To add a new
   global service (available for all bricks) please go in the
   subfolder services/. For more documentation please go on
   ROOT_PROJECT/doc/.  *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open PhTools (* Provide PhOpt, PhConfig, PhDebug... *)
}}
	     
(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let brick_verb =
  PhConfig.get_value_opt dico_conf "VERBOSE"
  |> PhOpt.map int_of_string |> PhOpt.get (-1)

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
let () =
  Kernel.Appli.register
    ~service:%%_ONE_WORD_%%Services.main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"coucou"
           ~css:[["css";"main_css.css"]]
           Html5.F.(body [
             h2 [pcdata "Hello"];
	     Raw.a ~a:[a_onclick {{say_hello}}] [pcdata (Printf.sprintf "Hello, please click on me ! You are in a page created by the brick %s." brick_name)];
	     p [pcdata (Printf.sprintf "You chose the word : %s. A silent configuration exists and is %s." example silent_conf)]
           ])));
