(* Documention in .eliomi *)

{shared{
module CCOpt = CCOpt
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let sp = Printf.sprintf
}}   

let (//) = FilePath.concat

(* The Config_tmp module is usefull because Config needs Debug,
   and Debug needs Config...  *)
module PhConfig_tmp = struct
  type dico = (string * string) list
  exception Config_item_not_found of string 

  (* These functions shouldn't be used by the user... There are present only for tests *)
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

  let to_opt f = try match f () with "" -> None | x -> Some x
    with Config_item_not_found _ -> None

  (** This is run only once when the library is loaded, so time O(n) isn't important *)
  let get_value dico key =
    try
      List.find (fun (k,v) -> k = key) dico |> snd
    with Not_found -> raise (Config_item_not_found key)

  let get_value_opt dico key = to_opt (fun () -> get_value dico key)

  (** Get a list from a value. Usefull to prefix an url : (Eg : forum/admin/) *)
  let get_value_list dico ?(sep="/") key =
    get_value dico key
    |> Str.(split (regexp sep))
  let get_value_list_opt dico ?(sep="/") key =
    match get_value_opt dico key with
      None -> None
    | Some s -> Some Str.(split (regexp sep) s)


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

  (* There are two mode functions in the Config module (just after Debug) *)
end

(* Getting conf. They are only local variables *)
let dico_conf = PhConfig_tmp.get_dico ()
let brick_name = PhConfig_tmp.get_value dico_conf "BRICK_NAME"
let brick_verb = PhConfig_tmp.get_value dico_conf "VERBOSE" |> int_of_string
let default_verbose =
  (PhConfig_tmp.get_value dico_conf "DEFAULT_VERBOSE" |> int_of_string )

    {client{
        let brick_name = %brick_name
let brick_verb = %brick_verb
let default_verbose = %default_verbose
}}

{shared{
module PhDebug = struct

  (** Debug mode :
      This module is supposed to give an easy way to print messages depending
      on the verbosity configured in the brick. When you want to print a message
      you must associate it to a verbosity between 1 and 10 (in code : verb) :
      - 1 : Only very important messages that the user cannot miss
      - 2 : Important messages
      - 3 : Messages not really important that occurs only once (for example the
      configuration of a brick just after loading)
      - 4 : Messages pretty rare that can be usefull to know and that won't flood the output
      - 5 : Frequent but pretty interesting messages
      - 6 : Frequent messages useful only when the user want's to debug a program
      - 7 : Really frequent messages
      - 8 : Display tons of useless messages

      There are two ways to configure
      the verbosity of the module :
      - Set in config/System/PhTools/main.dico the variable DEFAULT_VERBOSE to the
      verbosity you want to configure a default behavior
      - Set in config/<your brick>/main.dico a variable like VERBOSE to -1 to use
      the default verbose, or to overwrite the default one choose an other one.
      It will be only changed in the current brick. (brick_verb in code)

  *)

  (* It is initialy defined outside because it needs server
     specific functions *)
  let get_default_verbose () = default_verbose

  let get_verbose brick_verb =
    if brick_verb < 0 then get_default_verbose ()
    else brick_verb

  let can_write brick_verb verb =
    verb <= get_verbose brick_verb

  (* These functions consider PhTools and the user brick config *)
  let basic_printf brick_verb verb fmt =
    Printf.ksprintf
      (fun s ->
         if can_write brick_verb verb
         then Eliom_lib.debug "%s" s else ())
      fmt

  let basic_sprintf brick_verb verb fmt =
    Printf.ksprintf
      (fun s ->
         if can_write brick_verb verb
         then s else "")
      fmt

  (* These functions consider only the default config *)
  let basic_printf' verb fmt = basic_printf (-1) verb fmt
  let basic_sprintf' verb fmt = basic_sprintf (-1) verb fmt

  (* printf brick_name verb message *)
  let start_time = Sys.time ()
  let print_string brick_name brick_verb verb str =
    if can_write brick_verb verb
    then Eliom_lib.debug "[%s:%d][%.3f] %s"
        brick_name
        verb
        (Sys.time () -. start_time)
        str
    else ()
  let format_string brick_name brick_verb verb str =
    if can_write brick_verb verb
    then Printf.sprintf "[%s:%d][%.3f] %s"
        brick_name
        verb
        (Sys.time () -. start_time)
        str
    else ""
  let printf brick_name brick_verb verb fmt =
    Printf.ksprintf
      (print_string brick_name brick_verb verb)
      fmt
  let sprintf brick_name brick_verb verb fmt =
    Printf.ksprintf
      (format_string brick_name brick_verb verb)
      fmt
end
}}

module PhConfig = struct
  (* The Config_tmp module is usefull because Config needs Debug,
     and Debug needs Config...  *)
  include PhConfig_tmp

  let print_config brick_name brick_verb =
    PhDebug.printf brick_name
      brick_verb
      3
      "===== Configuration %s =====" brick_name;
    get_dico ()
    |> List.iter
      (fun (k,v) -> PhDebug.printf brick_name brick_verb 3 "%s :> %s" k v);
    PhDebug.printf brick_name brick_verb 3 ""

end

(** This module is usefull to get the path of some files. Beware this path
    is not the path in *)
module PhPath =
struct
  (** The folder that contains all the website *)
  let www = FileUtil.pwd ()
  (* TODO : find a better way because it can be configured in the ocsigen
     server config file *)
  (** This static folder must contain only default files that arent's
      supposed to change (Hence '[_cte]'). If you do any change it will
      be lost in the next website update. To avoid that please use the
      [static_user] folder instead, or the [data] folder. It is built
      from the [static/] folder of each brick. *)
  let static_cte =
    www // "static"

  (** This folder is the sub-folder in the constant static dir that should
      contain all static files of your brick.*)
  let static_cte_brick brick_name =
    static_cte // "bricks" // brick_name

  (** This static folder can be modified (the modifications will stay even
      after a website update). It is a symlink whose target is the folder
      [static/] at the root of the project (The symlink is in
      [www/local/var/static/] ). It can overwrite the [static_cte] folder
      when you try to access it from it's URL if a file is in both
      [static_cte] and [static_user]. Please don't use this folder
      but [static_user_brick] instead.*)
  let static_user =
    www // "local" // "var" // "static"

  (** This folder is the sub-folder in the user static dir that should
      contain all static files of your brick edited by the user. For example
      if the user wants to define a new css for the brick it should put it
      here.*)
  let static_user_brick brick_name =
    static_user // "bricks" // brick_name

  (** Use this function to get the path of an existing file. If returns
      the path in [static_user] if the file is present. If not it returns
      the one in [static_cte].
      @raise Not_found if the file is present nowhere *)
  let get_static_file brick_name filename =
    let uf = (static_user_brick brick_name) // filename in
    let bf = (static_cte_brick brick_name) // filename in
    if FileUtil.(test Exists uf) then uf
    else if FileUtil.(test Exists bf) then bf
    else raise Not_found

  (** The data folder. This folder isn't accessible to the user from
      it's browser and isn't removed when you update your website.
      Please don't use this folder but [data_brick] instead.*)
  let data =
    www // "local" // "var" // "data"

  (** Use this folder to put every data that shouln't be accessible to the
      user from the web and should stay . *)
  let data_brick brick_name =
    data // brick_name

  (** The log folder. *)
  let log = www // "local" // "var" // "log"

end 

(* Usefull to debug, remove it when you don't need that anymore *)
let _ =
  PhConfig.print_config brick_name brick_verb;
  Kernel.dire_bjr ()
