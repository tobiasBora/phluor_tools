(* You can put here you code. You have an example of configuration use, client code...
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}


(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let example = PhConfig.get_value dico_conf "EXAMPLE"
let silent_conf = PhConfig.get_value dico_conf "SILENT_CONF"
(* The prefix is used in service/ *)

(* Usefull to debug *)
let _ = PhConfig.list_config ()

(** It's really easy to use javascript : *)
{client{
     let say_hello _ =
       Eliom_lib.alert "Hello, I love your example word %s !" %example;
}}

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
	     Raw.a ~a:[a_onclick {{say_hello}}] [pcdata (Printf.sprintf "Hello, please click on me ! You are in a page created by the brick %%_NAMESPACE_%%/%%_SHORTNAME_%%.")];
	     p [pcdata (Printf.sprintf "You chose the word : %s. A silent configuration exists and is %s." example silent_conf)]
           ])));
