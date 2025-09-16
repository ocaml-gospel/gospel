(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module defines the type of resolved identifiers. This module differs
    from [Preid] in that these identifiers are coupled with a unique tag that
    can be used to check if two identifiers are the same. These identifiers are
    used during type unification to check if type names, type variables, etc...
    are equal. *)

module Tag : sig
  type t
  (** The type of unique identifiers. *)

  val equal : t -> t -> bool
  (** Equality over identifiers. *)

  val hash : t -> int
  (** Hash function for identifiers. *)

  val set_project_name : string -> unit
  (** This function should be called exactly once so that the type checker
      generates identifiers that are unique to this project. This means that
      when we use files from this project in other projects, there will be no
      clashes with regards to identifiers as long as both projects are named
      differently. *)

  val is_project : t -> string -> bool
  (** [is_project id project] checks if the tag [id] represents an identifier
      belonging to the provided [project]. *)
end

module IdTable : Hashtbl.S with type key = Tag.t

type t = {
  id_str : string; (* Variable name. Not used internally. *)
  id_fixity : Preid.fixity;
  id_attrs : string list; (* Variable attributes *)
  id_loc : Location.t;
  id_tag : Tag.t;
      (* Unique identifier. During typechecking, this is what
       Inferno uses to check if two variables are the same. *)
}

val pp : Format.formatter -> t -> unit

(* Functions needed for Inferno *)

val to_string : t -> string
val equal : t -> t -> bool
val hash : t -> int

(* ---------------------------------- *)

val mk_id : ?loc:Location.t -> string -> t
val stdlib_project : string
val stdlib_id : t

val is_stdlib : t -> bool
(** [is_stdlib id] checks if the identifier [id] refers to a name defined in the
    Gospel standard library. *)

val is_primitive : t -> bool
(** [is_primitive id] checks if the identifier [id] refers to a primitive Gospel
    name. *)

val from_preid : Preid.t -> t
(** [from_preid pid] Turns an identifier created during parsing into a uniquely
    tagged name. *)
