(* Here is a demo which shows how config and JS can be used easily
To edit the configuration, copy the content of the folder src/Test/Hello_conf_services/config inside config/Test/Hello_conf_services/, and edit the two names in the file main.dico
 *)

(* Conf *)
let dico = PhConfig.get_dico ()
let url = PhConfig.get_value_list dico "url"
let main_service = Eliom_service.App.service ~path:url ~get_params:Eliom_parameter.unit ()
