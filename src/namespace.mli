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
type fun_info = { fid : Ident.t }
type ty_info = { tid : Ident.t; tarity : int }
type mod_info = { mid : Ident.t; mdefs : mod_defs }

(* Functions to update the environment by adding a top level definition *)
val add_fun : env -> Ident.t -> env
val add_type : env -> Ident.t -> int -> env
val add_mod : env -> Ident.t -> mod_defs -> env

(* These functions receive a qualified identifier and return a fully resolved
   identifier and the information associated with it. *)
val type_info : mod_defs -> Parse_uast.qualid -> Id_uast.qualid * ty_info
val fun_qualid : mod_defs -> Parse_uast.qualid -> Id_uast.qualid

val empty_env : env
(** The empty environment. The only names in scope are Gospel primitive types *)

val submodule : env -> env
(** Returns a new environment for processing a submodule. The variables in scope
    remain the same and the set of definitions is empty. *)
