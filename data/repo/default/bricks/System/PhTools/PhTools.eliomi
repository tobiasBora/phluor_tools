{server{
    (** A set of libraries of usefull functions.*)

    (** A library written by compagnion_cube to deal with options.*)
module CCOpt :
sig
  (** {1 Options}
      Please note that the {!CCOpt} module is exactly
      the one provide by compagnion_cube in [containers] (that's why
      it needs [containers] to compile). I put a copy here because
      like that it's possible to have a local documention (just open
      [bricks_src/System/PhTools/doc/api] to find it) and it's easier to
      include it in both server and client code. Thank you for your
      library compagnion_cube. *)

  type +'a t = 'a option
  (** Transform the element inside, if any *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [maybe f x o] is [x] if [o] is [None], otherwise it's [f y] if [o = Some y] *)
  val maybe : ('a -> 'b) -> 'b -> 'a t -> 'b
  val is_some : _ t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Monadic return *)
  val return : 'a -> 'a t
  (** Infix version of {!map} *)
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Monadic bind *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Flip version of {!>>=} *)
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Iterate on 0 or 1 elements *)
  val iter : ('a -> unit) -> 'a t -> unit
  (** Fold on 0 or 1 elements *)
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Filter on 0 or 1 elements
      @since 0.5 *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [get default x] unwraps [x], but if [x = None] it returns [default] instead.
      @since 0.4.1 *)
  val get : 'a -> 'a t -> 'a
  (** Open the option, possibly failing if it is [None]
      @raise Invalid_argument if the option is [None] *)
  val get_exn : 'a t -> 'a
  (** [get_lazy default_fn x] unwraps [x], but if [x = None] it returns [default_fn ()] instead.
      @since 0.6.1 *)
  val get_lazy : (unit -> 'a) -> 'a t -> 'a
  (** [sequence_l [x1; x2; ...; xn]] returns [Some [y1;y2;...;yn]] if
      every [xi] is [Some yi]. Otherwise, if the list contains at least
      one [None], the result is [None]. *)
  val sequence_l : 'a t list -> 'a list t
  (** [wrap f x] calls [f x] and returns [Some y] if [f x = y]. If [f x] raises
      any exception, the result is [None]. This can be useful to wrap functions
      such as [Map.S.find].
      @param handler the exception handler, which returns [true] if the
      exception is to be caught. *)
  val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
  (** [wrap2 f x y] is similar to {!wrap} but for binary functions. *)
  val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option

  (** {2 Applicative} *)

  (** Alias to {!return} *)
  val pure : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** {2 Alternatives} *)
  (** [a <+> b] is [a] if [a] is [Some _], [b] otherwise *)
  val (<+>) : 'a t -> 'a t -> 'a t
  (** {2 Conversion and IO} *)
  (** [choice] returns the first non-[None] element of the list, or [None] *)
  val choice : 'a t list -> 'a t
  val to_list : 'a t -> 'a list
  (** Head of list, or [None] *)
  val of_list : 'a list -> 'a t
  type 'a sequence = ('a -> unit) -> unit
  type 'a gen = unit -> 'a option
  type 'a printer = Buffer.t -> 'a -> unit
  type 'a random_gen = Random.State.t -> 'a
  val random : 'a random_gen -> 'a t random_gen
  val to_gen : 'a t -> 'a gen
  val to_seq : 'a t -> 'a sequence
  val pp : 'a printer -> 'a t printer
end

(** This module is useful to print debug texts on the output.*)
module PhDebug :
sig
  (** {2 Full printf functions} *)

  (** [printf brick_name brick_verb verb fmt] will print the format [fmt] if [brick_verb <= verb], and the output will look like "[\[brickname:verb\]\[hour\] Message from fmt]".

      Usually you put at the top of your eliom file something like [let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt] to use a shorter version. *)
  val printf :
    string -> int -> int -> ('a, unit, string, unit) format4 -> 'a
  (** Same thing that printf but generate a string hi!!! *)
  val sprintf :
    string -> int -> int -> ('a, unit, string, string) format4 -> 'a
  (** Same thing that printf but take a string as argument instead of a format *)
  val print_string : string -> int -> int -> string -> unit
  (** Same thing that sprintf but take a string as argument *)
  val format_string : string -> int -> int -> string -> string

  (** {2 Useful informations} *)
  (** Get the default verbosity (defined in [config/System/PhTools/main.dico]). *)
  val get_default_verbose : unit -> int
  (** [get_verbose brick_verb] returns the current verbosity (if [brick_verb = -1] the default verbosity is chosen) *)
  val get_verbose : int -> int
  (** [can_write brick_verb verb] returns true if a text of priority [verb] can be printed. Usefull if you want to do a more complex thing than a simple print. *)
  val can_write : int -> int -> bool

  (** {2 Basic functions}
      Use them only if you know what you are doing. It is usually recommended to use {!printf} instead. *)
  (** Same as printf but doesn't display the brickname at the beginning. Not recommended to use if it's not for a good reason.*)
  val basic_printf : int -> int -> ('a, unit, string, unit) format4 -> 'a
  val basic_sprintf :
    int -> int -> ('a, unit, string, string) format4 -> 'a
  val basic_printf' : int -> ('a, unit, string, unit) format4 -> 'a
  val basic_sprintf' : int -> ('a, unit, string, string) format4 -> 'a
  (** The start time of the server *)
  val start_time : float
end

(** This module is useful to configure a brick.*)
module PhConfig :
sig
  (** {1 Configuration}
      This module is useful to configure a brick. Put in a dico file all variables in [config/<brick>/main.dico] (or if you want to package the brick in [bricks_src/<brick>/config_model/main.dico]) and you will be able to load it here. Please don't forget to also edit the file [config/<brick>/modules.xml] to apply any modification, and [bricks_src/<brick>/package/replacement.qdico] if you want to ask some questions to the user when you install the brick. *)

  (** {2 Get the dictionary} *)
  (** Error raised when you ask for an item not present in the config file. (usually you forgot to report in the [config/<brick>/modules.xml] file.) *)
  exception Config_item_not_found of string
  (** Main type. A dico is a set of couple (key,value) loaded from the xml configuration file. (inserted in [www/run_server.conf]) *)
  type dico = (string * string) list
  (** This function load a dico from the *)
  val get_dico : unit -> dico
  (** {2 Get the configuration }*)
  (** [get_value dico key] get in the dico the key [key].
      @raise Config_item_not_found [key] if the key isn't in the dico.
  *)
  val get_value : dico -> string -> string
  (** Same as [get] but return [None] if the value is empty or not present *)
  val get_value_opt : dico -> string -> string option
  (** [get_value_list dico ~sep key] will return a list of item that are separeted by [sep] (by default, [sep = '/']). It is useful to get a prefix url for example.
      @raise Config_item_not_found if the key isn't in the dico.
  *)
  val get_value_list : dico -> ?sep:string -> string -> string list
  val get_value_list_opt :
    dico -> ?sep:string -> string -> string list option
  (** Same as get_value, but return [true] if the value is in the set {true,yes,t,y,1} (the case isn't important), and [false] else.
      @raise Config_item_not_found if the key isn't in the dico.*)
  val get_bool : dico -> string -> bool
  (** [print_config brick_name brick_verb] will print all elements in the brick_dico in a pretty way. *)
  val print_config : string -> int -> unit
  (** {2 Advanced functions} *)
  (** Get the xml associated with the brick *)
  val get_xml : unit -> Simplexmlparser.xml list
  val dico_from_xml : Simplexmlparser.xml list -> dico
end

(** This module is usefull to get the path of some files. Beware this path
    is not the path in *)
module PhPath :
sig
  (** The folder that contains all the website *)
  val www : FilePath.filename
  (** This static folder must contain only default files that arent's
      supposed to change (Hence '[_cte]'). If you do any change it will
      be lost in the next website update. To avoid that please use the
      [static_user] folder instead, or the [data] folder. It is built
      from the [static/] folder of each brick. *)
  val static_cte : FilePath.filename
  (** This folder is the sub-folder in the constant static dir that should
      contain all static files of your brick.*)
  val static_cte_brick : FilePath.filename -> FilePath.filename
  (** This static folder can be modified (the modifications will stay even
      after a website update). It is a symlink whose target is the folder
      [static/] at the root of the project (The symlink is in
      [www/local/var/static/] ). It can overwrite the [static_cte] folder
      when you try to access it from it's URL if a file is in both
      [static_cte] and [static_user]. Please don't use this folder
      but [static_user_brick] instead.*)
  val static_user : FilePath.filename
  (** This folder is the sub-folder in the user static dir that should
      contain all static files of your brick edited by the user. For example
      if the user wants to define a new css for the brick it should put it
      here.*)
  val static_user_brick : FilePath.filename -> FilePath.filename
  (** Use this function to get the path of an existing file. If returns
      the path in [static_user] if the file is present. If not it returns
      the one in [static_cte].
      @raise Not_found if the file is present nowhere *)
  val get_static_file :
    FilePath.filename -> FilePath.filename -> FilePath.filename
  (** The data folder. This folder isn't accessible to the user from
      it's browser and isn't removed when you update your website.
      Please don't use this folder but [data_brick] instead.*)
  val data : FilePath.filename
  (** Use this folder to put every data that shouln't be accessible to the
      user from the web and should stay . *)
  val data_brick : FilePath.filename -> FilePath.filename
  (** The log folder. *)
  val log : FilePath.filename
end
(** Usefull shortcut to [Printf.sprintf] *)
val sp : ('a, unit, string) format -> 'a
(** Usefull shortcut to concatenate files. Use ["a" // "b"] instead of ["a/b"] *)
val ( // ) : FilePath.filename -> FilePath.filename -> FilePath.filename
val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t
}}
{client{
    (** A set of libraries of usefull functions.*)

    (** A library written by compagnion_cube to deal with options.*)
module CCOpt :
sig
  (** {1 Options}
       Please note that the {!CCOpt} module is exactly
       the one provide by compagnion_cube in [containers] (that's why
       it needs [containers] to compile). I put a copy here because
       like that it's possible to have a local documention (just open
       [bricks_src/System/PhTools/doc/api] to find it) and it's easier to
       include it in both server and client code. Thank you for your
       library compagnion_cube. *)

  type +'a t = 'a option
  (** Transform the element inside, if any *)
  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [maybe f x o] is [x] if [o] is [None], otherwise it's [f y] if [o = Some y] *)
  val maybe : ('a -> 'b) -> 'b -> 'a t -> 'b
  val is_some : _ t -> bool
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Monadic return *)
  val return : 'a -> 'a t
  (** Infix version of {!map} *)
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Monadic bind *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Flip version of {!>>=} *)
  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Iterate on 0 or 1 elements *)
  val iter : ('a -> unit) -> 'a t -> unit
  (** Fold on 0 or 1 elements *)
  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Filter on 0 or 1 elements
      @since 0.5 *)
  val filter : ('a -> bool) -> 'a t -> 'a t
  (** [get default x] unwraps [x], but if [x = None] it returns [default] instead.
      @since 0.4.1 *)
  val get : 'a -> 'a t -> 'a
  (** Open the option, possibly failing if it is [None]
      @raise Invalid_argument if the option is [None] *)
  val get_exn : 'a t -> 'a
  (** [get_lazy default_fn x] unwraps [x], but if [x = None] it returns [default_fn ()] instead.
      @since 0.6.1 *)
  val get_lazy : (unit -> 'a) -> 'a t -> 'a
  (** [sequence_l [x1; x2; ...; xn]] returns [Some [y1;y2;...;yn]] if
      every [xi] is [Some yi]. Otherwise, if the list contains at least
      one [None], the result is [None]. *)
  val sequence_l : 'a t list -> 'a list t
  (** [wrap f x] calls [f x] and returns [Some y] if [f x = y]. If [f x] raises
      any exception, the result is [None]. This can be useful to wrap functions
      such as [Map.S.find].
      @param handler the exception handler, which returns [true] if the
      exception is to be caught. *)
  val wrap : ?handler:(exn -> bool) -> ('a -> 'b) -> 'a -> 'b option
  (** [wrap2 f x y] is similar to {!wrap} but for binary functions. *)
  val wrap2 : ?handler:(exn -> bool) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c option

  (** {2 Applicative} *)

  (** Alias to {!return} *)
  val pure : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (<$>) : ('a -> 'b) -> 'a t -> 'b t
  (** {2 Alternatives} *)
  (** [a <+> b] is [a] if [a] is [Some _], [b] otherwise *)
  val (<+>) : 'a t -> 'a t -> 'a t
  (** {2 Conversion and IO} *)
  (** [choice] returns the first non-[None] element of the list, or [None] *)
  val choice : 'a t list -> 'a t
  val to_list : 'a t -> 'a list
  (** Head of list, or [None] *)
  val of_list : 'a list -> 'a t
  type 'a sequence = ('a -> unit) -> unit
  type 'a gen = unit -> 'a option
  type 'a printer = Buffer.t -> 'a -> unit
  type 'a random_gen = Random.State.t -> 'a
  val random : 'a random_gen -> 'a t random_gen
  val to_gen : 'a t -> 'a gen
  val to_seq : 'a t -> 'a sequence
  val pp : 'a printer -> 'a t printer
end
(** This module is useful to print debug texts on the output.*)
module PhDebug :
sig
  (** {2 Full printf functions} *)

  (** [printf brick_name brick_verb verb fmt] will print the format [fmt] if [brick_verb <= verb], and the output will look like "[\[brickname:verb\]\[hour\] Message from fmt]".

      Usually you put at the top of your eliom file something like [let printf verb fmt = PhDebug.printf brick_name brick_verb verb fmt] to use a shorter version. *)
  val printf :
    string -> int -> int -> ('a, unit, string, unit) format4 -> 'a
  (** Same thing that printf but generate a string hooo !*)
  val sprintf :
    string -> int -> int -> ('a, unit, string, string) format4 -> 'a
  (** Same thing that printf but take a string as argument instead of a format *)
  val print_string : string -> int -> int -> string -> unit
  (** Same thing that sprintf but take a string as argument *)
  val format_string : string -> int -> int -> string -> string

  (** {2 Useful informations} *)
  (** Get the default verbosity (defined in [config/System/PhTools/main.dico]). *)
  val get_default_verbose : unit -> int
  (** [get_verbose brick_verb] returns the current verbosity (if [brick_verb = -1] the default verbosity is chosen) *)
  val get_verbose : int -> int
  (** [can_write brick_verb verb] returns true if a text of priority [verb] can be printed. Usefull if you want to do a more complex thing than a simple print. *)
  val can_write : int -> int -> bool

  (** {2 Basic functions}
      Use them only if you know what you are doing. It is usually recommended to use {!printf} instead. *)
  (** Same as printf but doesn't display the brickname at the beginning. Not recommended to use if it's not for a good reason.*)
  val basic_printf : int -> int -> ('a, unit, string, unit) format4 -> 'a
  val basic_sprintf :
    int -> int -> ('a, unit, string, string) format4 -> 'a
  val basic_printf' : int -> ('a, unit, string, unit) format4 -> 'a
  val basic_sprintf' : int -> ('a, unit, string, string) format4 -> 'a
  (** The start time of the server *)
  val start_time : float
end
(** Usefull shortcut to [Printf.sprintf] *)
val sp : ('a, unit, string) format -> 'a
val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

}}
