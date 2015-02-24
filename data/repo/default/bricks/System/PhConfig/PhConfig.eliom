(* This library is aimed to provide an easy way to get configuration of modules.
The structure of the config file (in config/module.xml) should be :
<eliom module="modules/%%BRICK_NAME%%/server_part.cma">
  <config key="%%MYKEY%%" value="%%MYVALUE%%" />
</eliom>

 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

(* These functions shouldn't be used... There are present only for tests *)
let get_xml = Eliom_config.get_config
let dico_from_xml xml_list =
  let rec dico_from_xml_aux list = match list with
      [] -> []
    | (Simplexmlparser.Element ("config",list_k,_))::r ->
       begin
	 try
	   let key = List.find (fun (k1,v1) -> k1 = "key") list_k |> snd in
	   let value = List.find (fun (k2,v2) -> k2 = "value") list_k |> snd in
	   (key,value) :: (dico_from_xml_aux r)
	 with Not_found -> (dico_from_xml_aux r)
       end
    | x::r -> dico_from_xml_aux r
  in dico_from_xml_aux xml_list

(** Get the dico from the config file *)
let get_dico () = dico_from_xml (get_xml ())
				
(** This is run only once when the library is loaded, so time O(n) isn't important *)
let get_value dico key = List.find (fun (k,v) -> k = key) dico |> snd

(** Get a list from a value. Usefull to prefix an url : (Eg : forum/admin/) *)
let get_value_list dico ?(sep="/") key =
  get_value dico key
  |> Str.(split (regexp sep))

(** Get a bool from a value. The value can be (case insensitive)
- true, t, 1, yes, y
- false, f, 0, no, n
By default it returns false.
 *)
let get_bool dico key =
  let v = get_value dico key in
  Str.(string_match (regexp_case_fold "^true$") v 0)
  || Str.(string_match (regexp_case_fold "^[y|1|t]$") v 0)
  || Str.(string_match (regexp_case_fold "^yes$") v 0)

	
(** It's dirty but a bit quicker to use when there is only one key *)
let get_value_f key = let d = get_dico () in get_value d key
let get_value_list_f key = let d = get_dico () in get_value_list d key
									   
(** Usefull to debug *)
let list_dico dico =
  List.iter (fun (k,v) -> Eliom_lib.debug "%s :> %s" k v) dico
let list_config () =
  Eliom_lib.debug "-- Config options:";
  get_dico () |> list_dico
