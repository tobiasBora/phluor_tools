open Eliom_lib.Lwt_ops
open Eliom_lib
open Eliom_content
open PhTools (*Provide CCOpt, PhConfig and PhDebug *)

	      
(* ======================================== *)
(* ==========   Configuration   =========== *)
(* ======================================== *)

(** Getting configuration like password, username...  *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let brick_verb =
  PhConfig.get_value_opt dico_conf "VERBOSE"
  |> CCOpt.map int_of_string |> CCOpt.get (-1)
let unix_domain_socket_dir =
  PhConfig.get_value_opt dico_conf "UNIX_DOMAIN_SOCKET_DIR"
and host = PhConfig.get_value_opt dico_conf "HOST"
and port = match PhConfig.get_value_opt dico_conf "PORT" with
    Some x -> Some (int_of_string x)
  | None -> None
and user = PhConfig.get_value_opt dico_conf "USER"
and password =
  match PhConfig.get_value_opt dico_conf "PASSWORD_FILE" with
    Some filename ->
    (* The path is relative to the project root *)
    let file = if FilePath.is_relative filename then "../" ^ filename
	       else filename in
    let ic = open_in file in
    let tmp = input_line ic in
    (close_in ic; Some tmp)
  | None -> PhConfig.get_value_opt dico_conf "PASSWORD"
and database = PhConfig.get_value_opt dico_conf "DATABASE"

(* Define some usefull printf functions.
 Ex: (here 3 is the priority of the message) : printf 3 "Hello %s" name *)
let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt


(* ======================================== *)
(* ===========   Connection   ============= *)
(* ======================================== *)
exception No_such_resource
let (>>=) = Lwt.bind
module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query_ = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
module PGOCaml = Lwt_PGOCaml
      
let connect () =
  Lwt_PGOCaml.connect
    ?unix_domain_socket_dir
    ?user
    ?password
    ?database
    ?host
    ?port
    ()

let validate db =
  try_lwt
    lwt () = Lwt_PGOCaml.ping db in
    Lwt.return true
    with _ ->
      Lwt.return false

let transaction_block db f =
  Lwt_PGOCaml.begin_work db >>= fun _ ->
  try_lwt
    lwt r = f () in
    lwt () = Lwt_PGOCaml.commit db in
    Lwt.return r
    with e ->
      lwt () = Lwt_PGOCaml.rollback db in
    Lwt.fail e
	     
let pool : (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t =
  Lwt_pool.create 16 ~validate connect

(* This function is usefull to find a free connection and use it on
   a request *)
let full_transaction_block f =
  Lwt_pool.use pool (fun db -> transaction_block db (fun () -> f db))

(* This module allow you to choose your own dbh *)
module Lwt_Query = struct
  include Lwt_Query_
  let view_one dbh rq =
    try_lwt
      view_one dbh rq
    with Failure _ -> Lwt.fail No_such_resource

  let view_one_opt dbh view =
    try_lwt
      lwt answer = view_one dbh view in
      Lwt.return (Some (view_one answer))
    with No_such_resource -> Lwt.return None

  let alter dbh str = Lwt_PGOCaml.alter dbh str
end

(* But you can also use these function already linked with the above
   configured database (recommanded) *)
let view view =
  full_transaction_block (fun dbh ->
			  Lwt_Query.view dbh view)

let view_one view =
  full_transaction_block (fun dbh ->
			  Lwt_Query.view_one dbh view)

let view_one_opt view =
  full_transaction_block (fun dbh ->
			  Lwt_Query.view_one_opt dbh view)
let query query =
  full_transaction_block (fun dbh ->
			  Lwt_Query.query dbh query)

let value v = 
  full_transaction_block (fun dbh ->
			  Lwt_Query.value dbh v)

let value_opt v = 
  full_transaction_block (fun dbh ->
			  Lwt_Query.value_opt dbh v)

let alter str = 
  full_transaction_block (fun dbh ->
			  Lwt_Query.alter dbh str)
			 
(* ======================================== *)
(* ===============   Cache   ============== *)
(* ======================================== *)
	
(* See the module System/PhUsers to have one example *)
module type Cache_sig = sig
    type key_t
    type value_t
	   
    val has : key_t -> bool
    val set : key_t -> value_t -> unit
				    
    val reset : key_t -> unit
    val get : key_t -> value_t Lwt.t
    val wrap_function : key_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  end


module type Cache_f = sig
    module Make : functor
      (M : sig
             type key_t
             type value_t

             val compare : key_t -> key_t -> int
             val get : key_t -> value_t Lwt.t
	   end) -> Cache_sig with type key_t = M.key_t and type value_t = M.value_t
  end
		  
module Cache_f = struct
  module Make(M : sig
		    type key_t
		    type value_t

		    val compare : key_t -> key_t -> int
		    val get : key_t -> value_t Lwt.t
		  end) = struct
    type key_t = M.key_t
    type value_t = M.value_t

    (* we use an associative map to store the data *)
    module MMap = Map.Make(struct type t = M.key_t let compare = M.compare end)

    (* we use an eliom reference with the restrictive request scope, which is
     * sufficient and pretty safe (SECURITY), this permit to work on valid
     * data during the request *)
    let cache =
      Eliom_reference.Volatile.eref
	~scope:Eliom_common.request_scope
	MMap.empty

    let has k =
      let table = Eliom_reference.Volatile.get cache in
      try
	ignore (MMap.find k table);
	true
      with
      | Not_found -> false

    let set k v =
      let table = Eliom_reference.Volatile.get cache in
      Eliom_reference.Volatile.set cache (MMap.add k v table)

    let reset (k : M.key_t) =
      let table = Eliom_reference.Volatile.get cache in
      Eliom_reference.Volatile.set cache (MMap.remove k table)

    let get (k : M.key_t) =
      let table = Eliom_reference.Volatile.get cache in
      try Lwt.return (MMap.find k table)
      with
      | Not_found ->
	 try_lwt
           lwt ret = M.get k in
	Eliom_reference.Volatile.set cache (MMap.add k ret table);
	Lwt.return ret
	with _ -> Lwt.fail Not_found

    let wrap_function (k : M.key_t) f =
      (* we call the user function and we will reset the data correponding
       * to the key to be sure that we're going to use valid data with the
       * cache *)
      lwt ret = f () in
	let () = reset k in
	Lwt.return ret

  end
end

let _ =
  let open CCOpt in
  (* Usually it's generated with PhConfig.print_config, but not here
     because it's not stricly taken from the dictionnary, the data are
     manipulated before *)  
  printf 3 "===== Configuration Db (after parsing) =====";
  printf 3 "UNIX_DOMAIN_SOCKET_DIR :> %s"
	 (unix_domain_socket_dir |> get "None");
  printf 3 "HOST :> %s" (host |> get "None");
  printf 3 "PORT :> %s" (map string_of_int port |> get "None");
  printf 3 "USER :> %s" (user |> get "None");
  printf 3 "PASSWORD :> %s"
	 (if not (is_some password) then "None"
	  else "Not displayed for security reasons");
  printf 3 "DATABASE :> %s" (database |> get "None");
  printf 3 ""
