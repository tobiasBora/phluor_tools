let sp = Printf.sprintf
(* ======================================== *)
(* ===== Part I : BDD type definition ===== *)
(* ======================================== *)
(* Alter n'existe plus        *)
type gen_type =
    OString of (string * int)
  | OInt of int
  | OFloat of float
  | OBool of bool
  | SSerial
      
type mode_default = NoDefault | NullDefault | ValDefault
						
type column =
    {
      cl_name:string;
      cl_type:gen_type;
      cl_nullable:bool;
      cl_default:mode_default;
      cl_primary:bool
    }

type table =
    {
      tb_name:string;
      tb_columns:column list;
      tb_temp:bool; (* NOT IMPLEMENTED YET *)
      tb_remove_useless_column:bool;
    }

(* Output :
     (type used in sql creation TYPE ...
     ,type registered in database to compare type between them
     ,format in string the default value to be used in SET DEFAULT) *)
let sql_type_of_column table_name column =
  let quote s = sp  "'%s'" s in
  match column.cl_type with
    OString (str,size) -> (sp "character varying(%d)" size,
			   "character varying",
			   quote str)
  | OInt n -> ("bigint", "bigint", string_of_int n |> quote)
  | OFloat f -> ("double precision", "double precision",
		 string_of_float f |> quote)
  | OBool b -> ("boolean", "boolean", string_of_bool b |> quote )
  | SSerial -> ("integer", "integer", sp "nextval('%s_%s_seq')"
					 table_name
					 column.cl_name)
		 



(* =========================================== *)
(* ===== Part II : Ocaml code generation ===== *)
(* =========================================== *)

(* This part must create something like : *)
(* let table = <:table< test ( *)
(* 	     recette text NOT NULL, *)
(* 	     nom text NOT NULL, *)
(* 	     poids bigint NOT NULL DEFAULT($ <:value< 0L >> $), *)
(* 	     categorie text NOT NULL, *)
(* 	     DEFAULT($ <:value< "aucune categorie specifiee" >> $) *)
(* 									             ) >> *)
let gen_seq_part tbl =
  let sf = sp in
  let rec aux col_l = match col_l with
      [] -> ""
    | c::r ->
       (match c.cl_type with
	  SSerial -> sf "  let seq_%s = <:sequence< serial \"%s_%s_seq\">> in\n"
			c.cl_name
			tbl.tb_name
			c.cl_name
	| _ -> "") ^ (aux r)
  in aux tbl.tb_columns

(* What you must write in the
 <:table< tblname
   (column_name *type* DEFAULT (*default_value*) ) ...
 >>
 *)
let macaque_type_of_column col =
  let sf = sp in
  (match col.cl_type with
     OString (s,n) -> ("text", sf "$ <:value< \"%s\" >> $" s)
   | OInt n -> ("bigint",  sf "$ <:value< %dL >> $" n)
   | OFloat f -> ("double", sf "$ <:value< %fF >> $" f)
   | OBool b -> ("boolean", sf "$ <:value< %b >> $" b)
   | SSerial -> ("integer", sf "nextval $seq_%s$" col.cl_name))
  |> (fun (a,b) -> match col.cl_default with
		     NoDefault -> (a, "")
		   | NullDefault -> (a, "DEFAULT(null)") (* Does it work ??? *)
		   | ValDefault -> (a, sf "DEFAULT(%s)" b))

(* Gives the type to give to the macaque database (int32...) and the
   functions to convert it from/into the canonical object type (int) *)
let ocaml_type_of_column col =
  match col.cl_type with
    OString _ -> ("string", "", "")
  | OInt _ -> ("int64", "|> Int64.of_int", "|> Int64.to_int")
  | OFloat _ -> ("float", "", "")
  | OBool _ -> ("bool", "", "")
  | SSerial -> ("int32", "|> Int32.of_int", "|> Int32.to_int")
		   
let column_to_ocaml col =
  let (col_type,col_default) = macaque_type_of_column col in
  sp "   %s %s %s %s,\n"
     col.cl_name
     col_type
     (if col.cl_nullable then "" else "NOT NULL")
     col_default
     
let table_to_ocaml tbl =
  sp "let tbl_%s =\n%s\n  <:table< %s (\n%s\n)\n    >>\n\n"
     tbl.tb_name
     (gen_seq_part tbl)
     tbl.tb_name
     (List.map column_to_ocaml tbl.tb_columns
      |> List.fold_left (^) ""
      (* Remove end ',' *)
      |> (fun s -> String.sub s 0 ((String.length s) - 2)))

(* Here we try to provide a function like :
let <tbl_name>_create ~firstname ~lastname =
<:insert<
       $table_users$ := {
       uid = $table_users$?uid;
       firstname = $nullable_of_option_string u.firstname$;
       lastname = $nullable_of_option_string u.lastname$;
}>>
 *)

let macaque_fct_of_gen_type column =
  (match column.cl_type with
     OString _ -> "Sql.Value.string"
   | OInt _ -> "Int64.of_int |> Sql.Value.int64"
   | OFloat _ -> "Sql.Value.float"
   | OBool _ -> "Sql.Value.bool"
   | SSerial -> "(fun a -> a)")


let column_to_ocaml_create_arg column = match column.cl_type with
    SSerial -> ""
  | _ -> (if column.cl_nullable || column.cl_default <> NoDefault 
	  then "?" else "~")
	 ^ (column.cl_name)
      
let column_to_ocaml_create tbl column =
  if column.cl_type = SSerial then
    sp "      %s = $tbl_%s$?%s;\n"
       column.cl_name
       tbl.tb_name
       column.cl_name
  else 
    let fct = macaque_fct_of_gen_type column in
    sp "      %s = $%s$;\n"
       column.cl_name
       (match (column.cl_nullable, column.cl_default) with
	  true,ValDefault -> sp "opt_map (opt_map (fun s -> s |> %s)) %s |> opt_get (Some <:value<$tbl_%s$?%s>>) |> Sql.Op.of_option"
				fct
				column.cl_name
				tbl.tb_name
				column.cl_name
	| false,ValDefault -> sp "opt_map (fun s -> s |> %s) %s |> opt_get <:value<$tbl_%s$?%s>>"
				 fct
				 column.cl_name
				 tbl.tb_name
				 column.cl_name
	| true,_ -> sp "%s |> opt_map %s |> Sql.Op.of_option"
		       column.cl_name
		       fct
	| false,_ -> sp "%s |> %s"
			column.cl_name
			fct
       )
     
let table_to_ocaml_create tbl =
  ("let opt_get default v = match v with Some x -> x | None -> default\n")
  ^ ("let opt_map f v = match v with None -> None | Some x -> Some (f x)\n\n")
  ^ (sp "let create_%s%s () =\n  PhDb.query <:insert<\n    $tbl_%s$ := {\n%s\n    }\n  >>\n"
	tbl.tb_name
	(List.map column_to_ocaml_create_arg tbl.tb_columns
	 |> List.filter (fun s -> s <> "")
	 |> List.fold_left (fun a b -> a ^ " " ^ b) ""
	)
	tbl.tb_name
	(List.map (column_to_ocaml_create tbl) tbl.tb_columns
	 |> List.fold_left (^) ""
	 |> (fun s -> String.sub s 0 ((String.length s) - 2))
    ))

(* ======================================== *)
(* ===== Part IV : Shortcut functions ===== *)
(* ======================================== *)

(* These functions are usefull to have a clear code in database description *)
let cl_serial name =
  {
    cl_name=name;
    cl_type=SSerial;
    cl_nullable=false;
    cl_default=ValDefault;
    cl_primary=true
  }
  
let cl_string ?(length=255) ?(nullable=false) ?default name =
  {
    cl_name=name;
    cl_type=OString (CCOpt.get "" default, length);
    cl_nullable=nullable;
    cl_default=(if default = None then NoDefault else ValDefault);
    cl_primary=false
  }

let cl_bool ?(nullable=false) ?default name =
  {
    cl_name=name;
    cl_type=OBool (CCOpt.get false default);
    cl_nullable=nullable;
    cl_default=(if default = None then NoDefault else ValDefault);
    cl_primary=false
  }

let cl_int ?(nullable=false) ?default name =
  {
    cl_name=name;
    cl_type=OInt (CCOpt.get 0 default);
    cl_nullable=nullable;
    cl_default=(if default = None then NoDefault else ValDefault);
    cl_primary=false
  }


let cl_float ?(nullable=false) ?default name =
  {
    cl_name=name;
    cl_type=OFloat (CCOpt.get 0. default);
    cl_nullable=nullable;
    cl_default=(if default = None then NoDefault else ValDefault);
    cl_primary=false
  }
