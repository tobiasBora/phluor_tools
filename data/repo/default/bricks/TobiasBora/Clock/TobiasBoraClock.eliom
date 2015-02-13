(* You can put here you code. You have an example of configuration use, client code...
 *)

{shared{
  open Eliom_lib
  open Eliom_content
  open Html5
  module JsE = TobiasBoraEasyJS
}}


(* Getting conf : good way = use dico *)
(* let dico_conf = PhConfig.get_dico () *)
(* let example = PhConfig.get_value dico_conf "example" *)
(* let silent_conf = PhConfig.get_value dico_conf "silent_conf" *)

(* Usefull to debug, remove it when you don't need that anymore *)
(* let _ = PhConfig.list_config () *)

(** It's really easy to use javascript : *)
{client{
     let rec update_date h_elt mn_elt sec_elt =
       let wr x = Printf.sprintf "%02d" x in
       let h = JsE.getHours () in
       let mn = JsE.getMinutes () in
       let sec = JsE.getSeconds () in
       JsE.set_innerHTML h_elt (wr h);
       JsE.set_innerHTML mn_elt (wr mn);
       JsE.set_innerHTML sec_elt (wr sec);
       Lwt_js.sleep 1.
       >>= fun () -> update_date h_elt mn_elt sec_elt

}}

let classic_clock () =
  let hour_elt = D.(span [pcdata "Is JS enabled ?"])
  and mn_elt = D.(span [pcdata "Is JS enabled ?"])
  and sec_elt = D.(span [pcdata "Is JS enabled ?"]) in
  let _ = {unit{
	       Lwt.async (fun () -> update_date %hour_elt %mn_elt %sec_elt)
	  }}
  in
  (hour_elt, mn_elt, sec_elt)

let full_classic_clock () =
  let (hour_elt, mn_elt, sec_elt) = classic_clock () in
  let sep = F.(span [pcdata ":"]) in
  F.(div ~a:[a_class ["TobiasBoraClock_div"] ] [hour_elt;sep;mn_elt;sep;sec_elt])
