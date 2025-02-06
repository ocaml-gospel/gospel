(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib

(** {1 Pre-identifiers}

    These are not unique identifier; they come out of the parser and are used in
    the untyped AST. *)
module Preid : sig
  type t = private {
    pid_str : string;  (** The identifier name. *)
    pid_attrs : string list;  (** The attributes of the identifier. *)
    pid_loc : Location.t;  (** The location of the identifier. *)
  }
  (** The type for pre-identifiers. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty printer for pre-identifiers. *)

  val create : ?attrs:string list -> loc:Location.t -> string -> t
  (** [create ~attrs ~loc id] is a new pre-identifier identified with [id] with
      attributes [attrs] and location [loc]. Default attributes are empty, and
      default location is [Location.none]. *)

  val add_attr : t -> string -> t
  (** [add_attr t attr] is [t] with [attr] added to the list of its attributes.
  *)
end

(** {1 Identifiers}

    These are uniquely tagged identifiers produced by the typing. *)
module Ident : sig
  type t = private {
    id_str : string;  (** The identifier name. *)
    id_attrs : string list;  (** The attributes of the identifier. *)
    id_path : string list;  (** The full path of the identifier *)
    id_loc : Location.t;  (** The location of the identifier. *)
    id_tag : int;  (** The unique tag of the identifier. *)
  }
  (** The type for identifiers. *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val pp_simpl : Format.formatter -> t -> unit
  (** [pp fmt id] pretty prints [id] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt id] pretty prints [id] with their fully qualified name *)

  val create :
    ?attrs:string list -> ?path:string list -> loc:Location.t -> string -> t
  (** [create ~attrs ~path ~loc id] is a new pre-identifier identified with [id]
      with attributes [attrs], path [path] and location [loc]. A unique tag is
      automatically affected to the new identifier Default attributes are empty,
      and default location is [Location.none]. *)

  val of_preid : ?path:string list -> Preid.t -> t
  (** [of_preid ~path pid] is a fresh identifier using the same name, attributes
      and location as [pid]. If necessary, a [path] may also be supplied. A
      unique tag is automatically affected to the new identifier *)

  val set_loc : t -> Location.t -> t
  (** [set_loc t loc] is [t] with [loc] as its location. *)

  val add_attr : t -> string -> t
  (** [add_attr t attr] is [t] with [attr] added to the list of its attributes.
  *)
end

module Mid : Map.S with type key = Ident.t

(** {2 Hard-coded identifiers} *)

val eq : Ident.t
val neq : Ident.t
val not_id : Ident.t
val none : Ident.t
val some : Ident.t
val nil : Ident.t
val cons : Ident.t

(* Utils *)

val prefix : string -> string
val infix : string -> string
val mixfix : string -> string
val chain : string -> string
val is_prefix : string -> bool
val is_infix : string -> bool
val is_mixfix : string -> bool
val is_chain : string -> bool
