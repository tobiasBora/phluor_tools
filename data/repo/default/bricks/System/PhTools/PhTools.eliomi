(* WARNING `eliomc -i' generated this pretty ad-hoc - use with care! *)
{server{
  module PhOpt :
    sig
      exception Is_not_some
      val get : 'a -> 'a option -> 'a
      val get_exn : ?exn:exn -> 'a option -> 'a
      val map : ('a -> 'b) -> 'a option -> 'b option
      val is_none : 'a option -> bool
      val is_some : 'a option -> bool
    end
  module PhConfig_tmp :
    sig
      exception Config_item_not_found of string
      val get_xml : unit -> Simplexmlparser.xml list
      val dico_from_xml : Simplexmlparser.xml list -> (string * string) list
      val get_dico : unit -> (string * string) list
      val to_opt : (unit -> string) -> string option
      val get_value : (string * 'a) list -> string -> 'a
      val get_value_opt : (string * string) list -> string -> string option
      val get_value_list :
        (string * string) list -> ?sep:string -> string -> string list
      val get_value_list_opt :
        (string * string) list -> ?sep:string -> string -> string list option
      val get_bool : (string * string) list -> string -> bool
    end
  module PhDebug :
    sig
      val get_default_verbose : unit -> int
      val get_verbose : int -> int
      val can_write : int -> int -> bool
      val basic_printf : int -> int -> ('a, unit, string, unit) format4 -> 'a
      val basic_sprintf :
        int -> int -> ('a, unit, string, string) format4 -> 'a
      val basic_printf' : int -> ('a, unit, string, unit) format4 -> 'a
      val basic_sprintf' : int -> ('a, unit, string, string) format4 -> 'a
      val start_time : float
      val print_string : string -> int -> int -> string -> unit
      val format_string : string -> int -> int -> string -> string
      val printf :
        string -> int -> int -> ('a, unit, string, unit) format4 -> 'a
      val sprintf :
        string -> int -> int -> ('a, unit, string, string) format4 -> 'a
    end
  module PhConfig :
    sig
      exception Config_item_not_found of string
      val get_xml : unit -> Simplexmlparser.xml list
      val dico_from_xml : Simplexmlparser.xml list -> (string * string) list
      val get_dico : unit -> (string * string) list
      val to_opt : (unit -> string) -> string option
      val get_value : (string * 'a) list -> string -> 'a
      val get_value_opt : (string * string) list -> string -> string option
      val get_value_list :
        (string * string) list -> ?sep:string -> string -> string list
      val get_value_list_opt :
        (string * string) list -> ?sep:string -> string -> string list option
      val get_bool : (string * string) list -> string -> bool
      val print_config : string -> int -> unit
    end
}}
{client{
  module PhOpt :
    sig
      exception Is_not_some
      val get : 'a -> 'a option -> 'a
      val get_exn : ?exn:exn -> 'a option -> 'a
      val map : ('a -> 'b) -> 'a option -> 'b option
      val is_none : 'a option -> bool
      val is_some : 'a option -> bool
    end
  module PhDebug :
    sig
      val get_default_verbose : unit -> int
      val get_verbose : int -> int
      val can_write : int -> int -> bool
      val basic_printf : int -> int -> ('a, unit, string, unit) format4 -> 'a
      val basic_sprintf :
        int -> int -> ('a, unit, string, string) format4 -> 'a
      val basic_printf' : int -> ('a, unit, string, unit) format4 -> 'a
      val basic_sprintf' : int -> ('a, unit, string, string) format4 -> 'a
      val start_time : float
      val print_string : string -> int -> int -> string -> unit
      val format_string : string -> int -> int -> string -> string
      val printf :
        string -> int -> int -> ('a, unit, string, unit) format4 -> 'a
      val sprintf :
        string -> int -> int -> ('a, unit, string, string) format4 -> 'a
    end
}}
