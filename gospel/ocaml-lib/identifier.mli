(* pre-dentifiers *)

type attr = string

module Sattr : Set.S with type elt = attr

type preid = private {
  pid_str : string;
  pid_ats : attr list;
  pid_loc : Location.t;
}

module Hpid : Hashtbl.S with type key = preid

val create_pid    : string -> attr list -> Location.t -> preid
val pid_of_string : string -> preid
val pid_add_lab   : preid -> attr list -> preid

(* identifiers *)

type ident = private {
  id_str : string;
  id_ats : Sattr.t;
  id_loc : Location.t;
  id_tag : int;
}

module Hid : Hashtbl.S with type key = ident
module Mid : Map.S with type key = ident

val create_id   : string -> Sattr.t -> Location.t -> ident
val id_register : preid -> ident

val fresh_id          : string -> ident
val fresh_id_with_loc : string -> Location.t -> ident

val id_add_loc : Location.t -> ident -> ident
val id_add_lab : ident -> Sattr.elt -> ident

(* hard-coded ids *)

val eq   : ident
val neq  : ident
val lt   : ident
val le   : ident
val gt   : ident
val ge   : ident
val impl : ident
val plus : ident
val minus: ident
val mult : ident
val div  : ident
val umin : ident
val none : ident
val some : ident
val nil  : ident
val cons : ident

(* utils *)

val prefix : string -> string
val infix  : string -> string
val mixfix : string -> string

val is_prefix : string -> bool
val is_infix  : string -> bool
val is_mixfix : string -> bool

(* pretty-printer *)

val print_pid   : Format.formatter -> preid -> unit
val print_ident : Format.formatter -> ident -> unit
