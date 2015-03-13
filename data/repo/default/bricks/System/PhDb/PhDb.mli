exception No_such_resource
val ( >>= ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
module Lwt_thread :
  sig
    type 'a t = 'a Lwt.t
    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( =<< ) : ('a -> 'b t) -> 'a t -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( =|< ) : ('a -> 'b) -> 'a t -> 'b t
    val return_unit : unit t
    val return_none : 'a option t
    val return_nil : 'a list t
    val return_true : bool t
    val return_false : bool t
    type 'a key = 'a Lwt.key
    val new_key : unit -> 'a key
    val get : 'a key -> 'a option
    val with_value : 'a key -> 'a option -> (unit -> 'b) -> 'b
    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
    val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t
    val wrap : (unit -> 'a) -> 'a t
    val wrap1 : ('a -> 'b) -> 'a -> 'b t
    val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t
    val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd t
    val wrap4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e t
    val wrap5 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f t
    val wrap6 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g t
    val wrap7 :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h t
    val choose : 'a t list -> 'a t
    val nchoose : 'a t list -> 'a list t
    val nchoose_split : 'a t list -> ('a list * 'a t list) t
    val join : unit t list -> unit t
    val ( <?> ) : 'a t -> 'a t -> 'a t
    val ( <&> ) : unit t -> unit t -> unit t
    val async : (unit -> 'a t) -> unit
    val ignore_result : 'a t -> unit
    val async_exception_hook : (exn -> unit) ref
    type 'a u = 'a Lwt.u
    val wait : unit -> 'a t * 'a u
    val wakeup : 'a u -> 'a -> unit
    val wakeup_exn : 'a u -> exn -> unit
    val wakeup_later : 'a u -> 'a -> unit
    val wakeup_later_exn : 'a u -> exn -> unit
    val waiter_of_wakener : 'a u -> 'a t
    type 'a result = 'a Lwt.result
    val make_value : 'a -> 'a result
    val make_error : exn -> 'a result
    val of_result : 'a result -> 'a t
    val wakeup_result : 'a u -> 'a result -> unit
    val wakeup_later_result : 'a u -> 'a result -> unit
    type 'a state = 'a Lwt.state = Return of 'a | Fail of exn | Sleep
    val state : 'a t -> 'a state
    val is_sleeping : 'a t -> bool
    exception Canceled
    val task : unit -> 'a t * 'a u
    val on_cancel : 'a t -> (unit -> unit) -> unit
    val add_task_r : 'a u Lwt_sequence.t -> 'a t
    val add_task_l : 'a u Lwt_sequence.t -> 'a t
    val cancel : 'a t -> unit
    val pick : 'a t list -> 'a t
    val npick : 'a t list -> 'a list t
    val protected : 'a t -> 'a t
    val no_cancel : 'a t -> 'a t
    val pause : unit -> unit t
    val wakeup_paused : unit -> unit
    val paused_count : unit -> int
    val register_pause_notifier : (int -> unit) -> unit
    val on_success : 'a t -> ('a -> unit) -> unit
    val on_failure : 'a t -> (exn -> unit) -> unit
    val on_termination : 'a t -> (unit -> unit) -> unit
    val on_any : 'a t -> ('a -> unit) -> (exn -> unit) -> unit
    val poll : 'a t -> 'a option
    val apply : ('a -> 'b t) -> 'a -> 'b t
    val backtrace_bind : (exn -> exn) -> 'a t -> ('a -> 'b t) -> 'b t
    val backtrace_catch :
      (exn -> exn) -> (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    val backtrace_try_bind :
      (exn -> exn) -> (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
    val backtrace_finalize :
      (exn -> exn) -> (unit -> 'a t) -> (unit -> unit t) -> 'a t
    type in_channel = Lwt_io.input_channel
    val in_channel_of_descr : Lwt_unix.file_descr -> in_channel
    val make_in_channel :
      ?close:(unit -> unit Lwt.t) ->
      (string -> int -> int -> int Lwt.t) -> in_channel
    val input_line : in_channel -> string Lwt.t
    val input_value : in_channel -> 'a Lwt.t
    val input : in_channel -> string -> int -> int -> int Lwt.t
    val really_input : in_channel -> string -> int -> int -> unit Lwt.t
    val input_char : in_channel -> char Lwt.t
    val input_binary_int : in_channel -> int Lwt.t
    val open_in_gen :
      Unix.open_flag list -> int -> string -> in_channel Lwt.t
    val open_in : string -> in_channel Lwt.t
    val close_in : in_channel -> unit Lwt.t
    type out_channel = Lwt_io.output_channel
    val out_channel_of_descr : Lwt_unix.file_descr -> out_channel
    val make_out_channel :
      ?close:(unit -> unit Lwt.t) ->
      (string -> int -> int -> int Lwt.t) -> out_channel
    val output : out_channel -> string -> int -> int -> unit Lwt.t
    val flush : out_channel -> unit Lwt.t
    val output_string : out_channel -> string -> unit Lwt.t
    val output_value : out_channel -> 'a -> unit Lwt.t
    val output_char : out_channel -> char -> unit Lwt.t
    val output_binary_int : out_channel -> int -> unit Lwt.t
    val open_out_gen :
      Unix.open_flag list -> int -> string -> out_channel Lwt.t
    val open_out : string -> out_channel Lwt.t
    val close_out : out_channel -> unit Lwt.t
    val open_connection : Unix.sockaddr -> (in_channel * out_channel) Lwt.t
  end
module Lwt_PGOCaml :
  sig
    type 'a t = 'a PGOCaml_generic.Make(Lwt_thread).t
    type 'a monad = 'a Lwt_thread.t
    type isolation =
        [ `Read_committed
        | `Read_uncommitted
        | `Repeatable_read
        | `Serializable ]
    type access = [ `Read_only | `Read_write ]
    exception Error of string
    exception PostgreSQL_Error of string * (char * string) list
    val connect :
      ?host:string ->
      ?port:int ->
      ?user:string ->
      ?password:string ->
      ?database:string ->
      ?unix_domain_socket_dir:string -> unit -> 'a t monad
    val close : 'a t -> unit monad
    val ping : 'a t -> unit monad
    val alive : 'a t -> bool monad
    val begin_work :
      ?isolation:isolation ->
      ?access:access -> ?deferrable:bool -> 'a t -> unit monad
    val commit : 'a t -> unit monad
    val rollback : 'a t -> unit monad
    val transact :
      'a t ->
      ?isolation:isolation ->
      ?access:access -> ?deferrable:bool -> ('a t -> 'b monad) -> 'b monad
    val serial : 'a t -> string -> int64 monad
    val serial4 : 'a t -> string -> int32 monad
    val serial8 : 'a t -> string -> int64 monad
    val max_message_length : int ref
    val verbose : int ref
    val set_private_data : 'a t -> 'a -> unit
    val private_data : 'a t -> 'a
    val uuid : 'a t -> string
    type pa_pg_data = (string, bool) Hashtbl.t
    type oid = int32
    type param = string option
    type result = string option
    type row = result list
    val prepare :
      'a t ->
      query:string -> ?name:string -> ?types:oid list -> unit -> unit monad
    val execute_rev :
      'a t ->
      ?name:string ->
      ?portal:string -> params:param list -> unit -> row list monad
    val execute :
      'a t ->
      ?name:string ->
      ?portal:string -> params:param list -> unit -> row list monad
    val cursor :
      'a t ->
      ?name:string ->
      ?portal:string ->
      params:param list -> (row -> unit monad) -> unit monad
    val close_statement : 'a t -> ?name:string -> unit -> unit monad
    val close_portal : 'a t -> ?portal:string -> unit -> unit monad
    val inject : 'a t -> ?name:string -> string -> row list monad
    val alter : 'a t -> ?name:string -> string -> unit monad
    type row_description = result_description list
    and result_description =
      PGOCaml_generic.Make(Lwt_thread).result_description = {
      name : string;
      table : oid option;
      column : int option;
      field_type : oid;
      length : int;
      modifier : int32;
    }
    type params_description = param_description list
    and param_description =
      PGOCaml_generic.Make(Lwt_thread).param_description = {
      param_type : oid;
    }
    val describe_statement :
      'a t ->
      ?name:string ->
      unit -> (params_description * row_description option) monad
    val describe_portal :
      'a t -> ?portal:string -> unit -> row_description option monad
    val name_of_type : ?modifier:int32 -> oid -> string
    type inet = Unix.inet_addr * int
    type timestamptz = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t
    type int16 = int
    type bytea = string
    type point = float * float
    type hstore = (string * string option) list
    type numeric = string
    type bool_array = bool option list
    type int32_array = int32 option list
    type int64_array = int64 option list
    type string_array = string option list
    type float_array = float option list
    val string_of_oid : oid -> string
    val string_of_bool : bool -> string
    val string_of_int : int -> string
    val string_of_int16 : int16 -> string
    val string_of_int32 : int32 -> string
    val string_of_int64 : int64 -> string
    val string_of_float : float -> string
    val string_of_point : point -> string
    val string_of_hstore : hstore -> string
    val string_of_numeric : numeric -> string
    val string_of_inet : inet -> string
    val string_of_timestamp : CalendarLib.Calendar.t -> string
    val string_of_timestamptz : timestamptz -> string
    val string_of_date : CalendarLib.Date.t -> string
    val string_of_time : CalendarLib.Time.t -> string
    val string_of_interval : CalendarLib.Calendar.Period.t -> string
    val string_of_bytea : bytea -> string
    val string_of_string : string -> string
    val string_of_unit : unit -> string
    val string_of_bool_array : bool_array -> string
    val string_of_int32_array : int32_array -> string
    val string_of_int64_array : int64_array -> string
    val string_of_string_array : string_array -> string
    val string_of_float_array : float_array -> string
    val oid_of_string : string -> oid
    val bool_of_string : string -> bool
    val int_of_string : string -> int
    val int16_of_string : string -> int16
    val int32_of_string : string -> int32
    val int64_of_string : string -> int64
    val float_of_string : string -> float
    val point_of_string : string -> point
    val hstore_of_string : string -> hstore
    val numeric_of_string : string -> numeric
    val inet_of_string : string -> inet
    val timestamp_of_string : string -> CalendarLib.Calendar.t
    val timestamptz_of_string : string -> timestamptz
    val date_of_string : string -> CalendarLib.Date.t
    val time_of_string : string -> CalendarLib.Time.t
    val interval_of_string : string -> CalendarLib.Calendar.Period.t
    val bytea_of_string : string -> bytea
    val unit_of_string : string -> unit
    val bool_array_of_string : string -> bool_array
    val int32_array_of_string : string -> int32_array
    val int64_array_of_string : string -> int64_array
    val string_array_of_string : string -> string_array
    val float_array_of_string : string -> float_array
    val bind : 'a monad -> ('a -> 'b monad) -> 'b monad
    val return : 'a -> 'a monad
  end
module Lwt_Query_ :
  sig
    module PhDb :
      sig
        type 'a t = 'a Lwt_PGOCaml.t
        type 'a monad = 'a Lwt_thread.t
        type isolation =
            [ `Read_committed
            | `Read_uncommitted
            | `Repeatable_read
            | `Serializable ]
        type access = [ `Read_only | `Read_write ]
        exception Error of string
        exception PostgreSQL_Error of string * (char * string) list
        val connect :
          ?host:string ->
          ?port:int ->
          ?user:string ->
          ?password:string ->
          ?database:string ->
          ?unix_domain_socket_dir:string -> unit -> 'a t monad
        val close : 'a t -> unit monad
        val ping : 'a t -> unit monad
        val alive : 'a t -> bool monad
        val begin_work :
          ?isolation:isolation ->
          ?access:access -> ?deferrable:bool -> 'a t -> unit monad
        val commit : 'a t -> unit monad
        val rollback : 'a t -> unit monad
        val transact :
          'a t ->
          ?isolation:isolation ->
          ?access:access ->
          ?deferrable:bool -> ('a t -> 'b monad) -> 'b monad
        val serial : 'a t -> string -> int64 monad
        val serial4 : 'a t -> string -> int32 monad
        val serial8 : 'a t -> string -> int64 monad
        val max_message_length : int ref
        val verbose : int ref
        val set_private_data : 'a t -> 'a -> unit
        val private_data : 'a t -> 'a
        val uuid : 'a t -> string
        type pa_pg_data = (string, bool) Hashtbl.t
        type oid = int32
        type param = string option
        type result = string option
        type row = result list
        val prepare :
          'a t ->
          query:string ->
          ?name:string -> ?types:oid list -> unit -> unit monad
        val execute_rev :
          'a t ->
          ?name:string ->
          ?portal:string -> params:param list -> unit -> row list monad
        val execute :
          'a t ->
          ?name:string ->
          ?portal:string -> params:param list -> unit -> row list monad
        val cursor :
          'a t ->
          ?name:string ->
          ?portal:string ->
          params:param list -> (row -> unit monad) -> unit monad
        val close_statement : 'a t -> ?name:string -> unit -> unit monad
        val close_portal : 'a t -> ?portal:string -> unit -> unit monad
        val inject : 'a t -> ?name:string -> string -> row list monad
        val alter : 'a t -> ?name:string -> string -> unit monad
        type row_description = result_description list
        and result_description =
          Lwt_PGOCaml.result_description = {
          name : string;
          table : oid option;
          column : int option;
          field_type : oid;
          length : int;
          modifier : int32;
        }
        type params_description = param_description list
        and param_description =
          Lwt_PGOCaml.param_description = {
          param_type : oid;
        }
        val describe_statement :
          'a t ->
          ?name:string ->
          unit -> (params_description * row_description option) monad
        val describe_portal :
          'a t -> ?portal:string -> unit -> row_description option monad
        val name_of_type : ?modifier:int32 -> oid -> string
        type inet = Unix.inet_addr * int
        type timestamptz = CalendarLib.Calendar.t * CalendarLib.Time_Zone.t
        type int16 = int
        type bytea = string
        type point = float * float
        type hstore = (string * string option) list
        type numeric = string
        type bool_array = bool option list
        type int32_array = int32 option list
        type int64_array = int64 option list
        type string_array = string option list
        type float_array = float option list
        val string_of_oid : oid -> string
        val string_of_bool : bool -> string
        val string_of_int : int -> string
        val string_of_int16 : int16 -> string
        val string_of_int32 : int32 -> string
        val string_of_int64 : int64 -> string
        val string_of_float : float -> string
        val string_of_point : point -> string
        val string_of_hstore : hstore -> string
        val string_of_numeric : numeric -> string
        val string_of_inet : inet -> string
        val string_of_timestamp : CalendarLib.Calendar.t -> string
        val string_of_timestamptz : timestamptz -> string
        val string_of_date : CalendarLib.Date.t -> string
        val string_of_time : CalendarLib.Time.t -> string
        val string_of_interval : CalendarLib.Calendar.Period.t -> string
        val string_of_bytea : bytea -> string
        val string_of_string : string -> string
        val string_of_unit : unit -> string
        val string_of_bool_array : bool_array -> string
        val string_of_int32_array : int32_array -> string
        val string_of_int64_array : int64_array -> string
        val string_of_string_array : string_array -> string
        val string_of_float_array : float_array -> string
        val oid_of_string : string -> oid
        val bool_of_string : string -> bool
        val int_of_string : string -> int
        val int16_of_string : string -> int16
        val int32_of_string : string -> int32
        val int64_of_string : string -> int64
        val float_of_string : string -> float
        val point_of_string : string -> point
        val hstore_of_string : string -> hstore
        val numeric_of_string : string -> numeric
        val inet_of_string : string -> inet
        val timestamp_of_string : string -> CalendarLib.Calendar.t
        val timestamptz_of_string : string -> timestamptz
        val date_of_string : string -> CalendarLib.Date.t
        val time_of_string : string -> CalendarLib.Time.t
        val interval_of_string : string -> CalendarLib.Calendar.Period.t
        val bytea_of_string : string -> bytea
        val unit_of_string : string -> unit
        val bool_array_of_string : string -> bool_array
        val int32_array_of_string : string -> int32_array
        val int64_array_of_string : string -> int64_array
        val string_array_of_string : string -> string_array
        val float_array_of_string : string -> float_array
        val bind : 'a monad -> ('a -> 'b monad) -> 'b monad
        val return : 'a -> 'a monad
      end
    val query : 'b PhDb.t -> ?log:out_channel -> 'a Sql.query -> 'a PhDb.monad
    val view :
      'b PhDb.t -> ?log:out_channel -> ('a, 'c) Sql.view -> 'a list PhDb.monad
    val view_one :
      'b PhDb.t -> ?log:out_channel -> ('a, 'c) Sql.view -> 'a PhDb.monad
    val view_opt :
      'b PhDb.t -> ?log:out_channel -> ('a, 'c) Sql.view -> 'a option PhDb.monad
    val value :
      'b PhDb.t ->
      ?log:out_channel ->
      < nul : Sql.non_nullable; t : 'a #Sql.type_info; .. > Sql.t ->
      'a PhDb.monad
    val value_opt :
      'b PhDb.t ->
      ?log:out_channel ->
      < nul : Sql.nullable; t : 'a #Sql.type_info; .. > Sql.t ->
      'a option PhDb.monad
  end
module PGOCaml = Lwt_PGOCaml
val connect : unit -> 'a Lwt_PGOCaml.t Lwt_PGOCaml.monad
val validate : 'a Lwt_PGOCaml.t -> bool Lwt.t
val transaction_block : 'a Lwt_PGOCaml.t -> (unit -> 'b Lwt.t) -> 'b Lwt.t
val pool : (string, bool) Hashtbl.t Lwt_PGOCaml.t Lwt_pool.t
val full_transaction_block :
  ((string, bool) Hashtbl.t Lwt_PGOCaml.t -> 'a Lwt.t) -> 'a Lwt.t
module Lwt_Query :
  sig
    module Db = Lwt_Query_.Db
    val query : 'b PhDb.t -> ?log:out_channel -> 'a Sql.query -> 'a PhDb.monad
    val view :
      'b PhDb.t -> ?log:out_channel -> ('a, 'c) Sql.view -> 'a list PhDb.monad
    val view_opt :
      'b PhDb.t -> ?log:out_channel -> ('a, 'c) Sql.view -> 'a option PhDb.monad
    val value :
      'b PhDb.t ->
      ?log:out_channel ->
      < nul : Sql.non_nullable; t : 'a #Sql.type_info; .. > Sql.t ->
      'a PhDb.monad
    val value_opt :
      'b PhDb.t ->
      ?log:out_channel ->
      < nul : Sql.nullable; t : 'a #Sql.type_info; .. > Sql.t ->
      'a option PhDb.monad
    val view_one : 'a PhDb.t -> ('b, 'c) Sql.view -> 'b Lwt.t
    val view_one_opt :
      'a PhDb.t ->
      ('b PhDb.t, 'c) Sql.view -> (('d, 'e) Sql.view -> 'd Lwt.t) option Lwt.t
    val alter : 'a Lwt_PGOCaml.t -> string -> unit Lwt_PGOCaml.monad
  end
val view : ('a, 'b) Sql.view -> 'a list Lwt.t
val view_one : ('a, 'b) Sql.view -> 'a Lwt.t
val view_one_opt :
  ('a Lwt_Query.PhDb.t, 'b) Sql.view ->
  (('c, 'd) Sql.view -> 'c Lwt.t) option Lwt.t
val query : 'a Sql.query -> 'a Lwt.t
val value :
  < nul : Sql.non_nullable; t : 'a #Sql.type_info; .. > Sql.t -> 'a Lwt.t
val value_opt :
  < nul : Sql.nullable; t : 'a #Sql.type_info; .. > Sql.t -> 'a option Lwt.t
val alter : string -> unit Lwt.t
module type Cache_sig =
  sig
    type key_t
    type value_t
    val has : key_t -> bool
    val set : key_t -> value_t -> unit
    val reset : key_t -> unit
    val get : key_t -> value_t Lwt.t
    val wrap_function : key_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  end
module type Cache_f =
  sig
    module Make :
      functor
        (M : sig
               type key_t
               type value_t
               val compare : key_t -> key_t -> int
               val get : key_t -> value_t Lwt.t
             end) ->
        sig
          type key_t = M.key_t
          type value_t = M.value_t
          val has : key_t -> bool
          val set : key_t -> value_t -> unit
          val reset : key_t -> unit
          val get : key_t -> value_t Lwt.t
          val wrap_function : key_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
        end
  end
module Cache_f :
  sig
    module Make :
      functor
        (M : sig
               type key_t
               type value_t
               val compare : key_t -> key_t -> int
               val get : key_t -> value_t Lwt.t
             end) ->
        sig
          type key_t = M.key_t
          type value_t = M.value_t
          module MMap :
            sig
              type key = M.key_t
              type +'a t
              val empty : 'a t
              val is_empty : 'a t -> bool
              val mem : key -> 'a t -> bool
              val add : key -> 'a -> 'a t -> 'a t
              val singleton : key -> 'a -> 'a t
              val remove : key -> 'a t -> 'a t
              val merge :
                (key -> 'a option -> 'b option -> 'c option) ->
                'a t -> 'b t -> 'c t
              val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
              val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
              val iter : (key -> 'a -> unit) -> 'a t -> unit
              val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
              val for_all : (key -> 'a -> bool) -> 'a t -> bool
              val exists : (key -> 'a -> bool) -> 'a t -> bool
              val filter : (key -> 'a -> bool) -> 'a t -> 'a t
              val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
              val cardinal : 'a t -> int
              val bindings : 'a t -> (key * 'a) list
              val min_binding : 'a t -> key * 'a
              val max_binding : 'a t -> key * 'a
              val choose : 'a t -> key * 'a
              val split : key -> 'a t -> 'a t * 'a option * 'a t
              val find : key -> 'a t -> 'a
              val map : ('a -> 'b) -> 'a t -> 'b t
              val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
            end
          val cache : M.value_t MMap.t Eliom_reference.Volatile.eref
          val has : MMap.key -> bool
          val set : MMap.key -> M.value_t -> unit
          val reset : M.key_t -> unit
          val get : M.key_t -> M.value_t Lwt.t
          val wrap_function : M.key_t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
        end
  end
