(* You can put here you code. You have an example of configuration use, client code...
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

(** It's really easy to use javascript : *)
{client{
     let set_innerHTML elt str =
       let elt_dom = Html5.To_dom.of_element elt in
       elt_dom##innerHTML <- (Js.string str)
			       
     let get_innerHTML elt =
       let elt_dom = Html5.To_dom.of_element elt in
       Js.to_string (elt_dom##innerHTML)

     (** Deal with hours *)
     let getFullYear () =
       let date = jsnew Js.date_now () in
       date##getFullYear ()
			 
     let getMonth () =
       let date = jsnew Js.date_now () in
       date##getMonth ()
		      
     let getDate () =
       let date = jsnew Js.date_now () in
       date##getDate ()
		     
     let getDay () =
       let date = jsnew Js.date_now () in
       date##getDay ()
		    
     let getHours () =
       let date = jsnew Js.date_now () in
       date##getHours ()
		      
     let getMinutes () =
       let date = jsnew Js.date_now () in
       date##getMinutes ()
			
     let getSeconds () =
       let date = jsnew Js.date_now () in
       date##getSeconds ()
			
}}

(* And you can too create server functions *)
let give_me_five () = 5
