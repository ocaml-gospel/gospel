(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** This module provides operations to keep track of all names defined in the
    top level. *)

type env
(** The (immutable) environment that keeps track of the definitions in the top
    level. *)

type mod_defs
(** A set of top level definitions *)

val scope : env -> mod_defs
(** Gets the set of definitions in scope *)

val defs : env -> mod_defs
(** Gets the set of definitions contained within the submodule we are currently
    processing. *)

(* Records containing the information necessary regarding each top level
    definitions *)
type mod_info = { mid : Ident.t; mdefs : mod_defs }

(* Functions to update the environment by adding a top level definition *)
val add_fun : env -> Ident.t -> Types.ty -> env
val add_type : env -> Ident.t -> Ident.t list -> Types.ty option -> env
val add_mod : env -> Ident.t -> mod_defs -> env

val resolve_alias : mod_defs -> Parse_uast.qualid -> Types.ty list -> Types.ty
(** If the type [q] in an alias such as

    [type (tv_1, tv_2, ...) t = alias]

    [resolve_alias env q l] returns the type expression [alias] where each
    occurrence of [tv_i] has been replaced with [List.nth l i]. If [q] is not an
    alias, return [q] applied to [l]. This function will raise a Gospel
    exception if the size of list [l] is not equal to the amount of type
    parameters this type receives. *)

val fun_qualid :
  mod_defs -> Parse_uast.qualid -> Id_uast.qualid * Ident.t list * Id_uast.pty
(** [fun_qualid defs q] turns every sub identifier in [q] into a fully resolved
    function identifier. Also returns the function's type and the type
    parameters used.
    @raise Not_found if [q] is not a valid function identifier *)

val empty_env : env
(** The empty environment. The only names in scope are Gospel primitive types *)

val submodule : env -> env
(** Returns a new environment for processing a submodule. The variables in scope
    remain the same and the set of definitions is empty. *)
