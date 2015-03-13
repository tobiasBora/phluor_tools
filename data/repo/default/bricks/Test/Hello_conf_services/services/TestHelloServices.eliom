(* You can add here every service that should be accessible to others bricks.
To load it they just need to add a dependence to %%_NAMESPACE_%%/%%_SHORTNAME_%%/services and call the services included in %%_ONE_WORD_%%Services
*)

open PhTools (* Provide PhOpt, PhConfig, PhDebug... *)

(* Conf *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let brick_verb =
  PhConfig.get_value_opt dico_conf "VERBOSE"
  |> PhOpt.map int_of_string |> PhOpt.get (-1)
let prefix_url = PhConfig.get_value_list dico_conf "PREFIX_URL"

(* Create a nice printf debug function *)
(* Ex: (here 3 is the priority of the message, see PhTools doc for
 more details) : printf 3 "Hello %s" name *)
let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt

(* ========================================== *)
(* ============   Service List   ============ *)
(* ========================================== *)

(* One url accessible to the prefix_url configured in the conf files (default funny/prefix *)
let main_service = Eliom_service.App.service ~path:(prefix_url @ [""]) ~get_params:Eliom_parameter.unit ()
