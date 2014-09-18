(* Here is a demo which shows how config and JS can be used easily
To edit the configuration, copy the content of the folder src/Test/Hello_conf_services/config inside config/Test/Hello_conf_services/, and edit the two names in the file main.dico
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}


(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let name = PhConfig.get_value dico_conf "name"
(* Getting conf without dico : faster but dirty *)
let name_dirty = PhConfig.get_value_f "name_dirty"
(* Usefull for debug *)
let _ = PhConfig.list_config ()

(** It's really easy to use javascript : *)
{client{
     let say_hello _ =
       Eliom_lib.alert "Hello %s !" %name_dirty;
}}

let () =
  Kernel.Appli.register
    ~service:TestHelloServices.main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"coucou"
           ~css:[["css";"main_css.css"]]
           Html5.F.(body [
             h2 [pcdata "Hello"];
	     Raw.a ~a:[a_onclick {{say_hello}}] [pcdata (Printf.sprintf "Hello %s, please click on me !" name)];
           ])));
