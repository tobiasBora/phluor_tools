(* Note that some code is inspired by eliom-base-app, but beware *)
(* it's not exactly the same code *)

{shared{
     open Eliom_content.Html5.F
     open PhTools (* Provide PhDebug, CCOpt, PhConfig *)
     exception Already_exists
     exception No_such_user
     let (>>=) = Lwt.(>>=)
     let (>|=) = Lwt.(>|=)
}}

(* Getting conf : good way = use dico *)
let dico_conf = PhConfig.get_dico ()
let brick_name = PhConfig.get_value dico_conf "BRICK_NAME"
let brick_verb =
  PhConfig.get_value_opt dico_conf "VERBOSE"
  |> CCOpt.map int_of_string |> CCOpt.get (-1)

let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt
{client{
     let printf verb fmt = PhDebug.printf %brick_name %brick_verb verb fmt
}}

	       
{shared{
     (** The type which represents a user.
         The password isn't registrated here for security reason
      *)
     type t = {
       uid : int64;
       firstname : string option;
       lastname : string option;
       nickname : string;
       email : string option;
       validated : bool;
       avatar : string option;
     } deriving (Json)
}}


{shared{
     (** The type which represents a user. *)
     let empty =
       {
	 uid=0L;
	 firstname=None;
	 lastname=None;
	 nickname="";
	 email=None;
	 validated=false;
	 avatar=Some "";
       }
}}

let user_of_userv v =
  {
    uid = v#!uid;
    firstname = v#?firstname;
    lastname = v#?lastname;
    nickname = v#!nickname;
    email = v#?email;
    validated = v#!validated;
    avatar = v#?avatar
  }
	    
(** Getters functions. *)
(* #! is the equivalent of '.' in record type for a Macaque view *)
let uid_of_user u = u.uid
let firstname_of_user u = u.firstname
let lastname_of_user u = u.lastname
let nickname_of_user u = u.nickname
let email_of_user u = u.email
let validated_of_user u = u.validated

let avatar_of_user u =
  match u.avatar with
    | None -> "default_avatar"
    | Some avatar -> avatar

(* TODO : Give (and test) the good uri
 => create a new brick to deal with paths *)
let avatar_uri_of_avatar avatar =
  Eliom_content.Html5.F.make_uri
    ~service:(Eliom_service.static_dir ()) ["avatars"; avatar]

let avatar_uri_of_user user =
  avatar_uri_of_avatar (avatar_of_user user)

module PhUsers_db = struct
  (* Those functions shouldn't be used by a user, they directly deal with
the database *)
  let table_users =
    let seq = <:sequence< bigserial "users_uid_seq">> in
    <:table< users (
     uid bigint NOT NULL DEFAULT (nextval $seq$),
     firstname text,
     lastname text,
     nickname text NOT NULL,
     password text,
     email text,
     validated boolean NOT NULL,
     avatar text
     ) >>


  (* Getters *)
  let userv_of_uid uid =
    PhDb.view_one
      <:view< {t.uid; t.firstname; t.lastname; t.nickname; t.password; t.email; t.validated; t.avatar} | t in $table_users$ ; t.uid = $int64:uid$ >>

  let userv_of_nickname nickname =
    PhDb.view_one
      <:view<
       {t.uid; t.firstname; t.lastname; t.nickname;
       t.password; t.email; t.validated; t.avatar}
       | t in $table_users$; t.nickname= $string:nickname$ >>
      
  (* Setters *)
  let create_user ?password u =
    let password_hash = match password with
	None -> None
      | Some x -> Some (Bcrypt.string_of_hash (Bcrypt.hash x))
    in

    (* TODO : Put this kind of function in PhDb *)
    let nullable_of_option_string opt =
      (match opt with
	 None -> None
       | Some x -> Some (Sql.Value.string x))
      |> Sql.Op.of_option
    in
    PhDb.query
      <:insert<
       $table_users$ := {
       uid = $table_users$?uid;
       firstname = $nullable_of_option_string u.firstname$;
       lastname = $nullable_of_option_string u.lastname$;
       nickname = $string: u.nickname$;
       password = $nullable_of_option_string password_hash$;
       email = $nullable_of_option_string u.email$;
       validated = $bool:true$;
       avatar = $nullable_of_option_string u.avatar$
       }
       >>


end

(* Using cache tools to prevent multiple same database queries
 * during the request. *)
module MCache =
  PhDb.Cache_f.Make(
      struct
	type key_t = int64
	type value_t = t
			 
	let compare = compare
	let get uid =
	  try_lwt
	    Eliom_lib.debug "Reset value";
	    lwt userv = PhUsers_db.userv_of_uid uid in
	    Lwt.return (user_of_userv userv)
	  with PhDb.No_such_resource -> Lwt.fail No_such_user
      end)
			
let user_of_uid uid = MCache.get uid
let user_of_nickname nickname =
  try_lwt
    printf 7 "User of nickname : %s" nickname;
    PhUsers_db.userv_of_nickname nickname >|= user_of_userv
  with PhDb.No_such_resource -> Lwt.fail No_such_user

(** Functions with ..._user wait for a user, and only ... contains all
configuration in named parameters
 *)
(** Add a new user in the database. May raise [Already_exists]*)
let create_user ?password user =
  try_lwt
    printf 6 "The user %s is going to be added..." user.nickname;
    lwt _ = user_of_nickname user.nickname in
    printf 4 "The user %s is already present." user.nickname;
    Lwt.fail Already_exists
  with No_such_user ->
    (printf 6 "The user %s doesn't exist yet." user.nickname;
     let _ = PhUsers_db.create_user ?password user in
     printf 4 "The user %s has been created." user.nickname;
    Lwt.return ())


(* Same that create_user but the options are in the parameters *)
let create ?firstname ?lastname ?password ?email ~validated ?avatar nickname =
  create_user
    ?password
    {
      empty with
      firstname;
      lastname;
      email;
      validated;
      avatar;
      nickname;
    }

let _ =
  PhConfig.print_config brick_name brick_verb
