(* You can put here you code. You have an example of configuration use, client code...
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}


(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let example = PhConfig.get_value dico_conf "example"
let silent_conf = PhConfig.get_value dico_conf "silent_conf"

(* Usefull to debug, remove it when you don't need that anymore *)
let _ = PhConfig.list_config ()

(** It's really easy to use javascript : *)
{client{
     (* You can use such client functions to make them available
        to the whole website
      *)
     let say_hello _ =
       Eliom_lib.alert "Hello, I love your example word %s !" %example;
}}

(* And you can too create server functions *)
let give_me_five () = 5
