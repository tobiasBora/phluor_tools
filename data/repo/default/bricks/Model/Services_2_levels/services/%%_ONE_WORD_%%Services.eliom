(* You can add here every service that should be accessible to others bricks.
To load it they just need to add a dependence to %%_NAMESPACE_%%/%%_SHORTNAME_%%/services and call the services included in %%_ONE_WORD_%%Services
*)

(* Conf *)
let dico = PhConfig.get_dico ()
let prefix_url = PhConfig.get_value_list dico "prefix_url"

(* One url accessible to the prefix_url configured in the conf files (default funny/prefix *)
let main_service = Eliom_service.App.service ~path:(prefix_url @ [""]) ~get_params:Eliom_parameter.unit ()
