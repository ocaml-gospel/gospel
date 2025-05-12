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

type record_info = {
  rid : Ident.t; (* The name of the record type. *)
  rparams : Ident.t list;
  (* The type parameters for the record type. Should be equal to the [tparams]
       list in the entry for [rid] in the corresponding type environment. *)
  rfields : (Ident.t * Id_uast.pty) list; (* The list of all record fields. *)
}
(** [record_info] contains the necessary to type check field accesses and record
    creation. When a record type is processed, an entry of [record_info] is
    added as well as an entry of [type_info]. *)

(* Functions to update the environment by adding a top level definition *)
val add_fun : env -> Ident.t -> Id_uast.pty -> env
val add_ocaml_val : env -> Ident.t -> Id_uast.pty -> env

val add_gospel_type :
  env -> Ident.t -> Ident.t list -> Id_uast.pty option -> env

val add_ocaml_type :
  env ->
  Ident.t ->
  Ident.t list ->
  mut:bool ->
  Id_uast.pty option ->
  Id_uast.pty option ->
  env

val add_record :
  env -> Ident.t -> Ident.t list -> (Ident.t * Id_uast.pty) list -> env

val add_exn : env -> Ident.t -> Id_uast.pty list -> env
(** [add_exn env id args] adds an exception named [id] to [env] that receives
    arguments of type [args].

    Note: If [alias] is [Some e], [args] should be the same argument list as
    [e]. *)

val add_mod : env -> Ident.t -> mod_defs -> env

val resolve_application :
  ocaml:bool ->
  mod_defs ->
  Parse_uast.qualid ->
  Id_uast.pty list ->
  Id_uast.app_info
(** If the type name [q] in an alias such as

    [type (tv_1, tv_2, ...) q = alias]

    [resolve_application ~ocaml env q l], where [l] are the type parameters of
    [q]. Returns a type application where the [app_alias] field is the
    expression [Some alias] where each occurrence of [tv_i] has been replaced
    with [List.nth l i]. If [q] is not an alias the [app_alias] field is [None].

    Similarly, if [q] and each type parameter in [l] has a logical
    representation [model], [app_model] is set to [model] where every occurrence
    [tv_i] has been replaced with the logical representation of [List.nth l i].
    Otherwise, [None].

    The [ocaml] flag is used to indicate which namespace should be searched.

    This function will raise a Gospel exception if the size of list [l] is not
    equal to the amount of type parameters this type receives.

    Postcondition: If [ocaml] is [false], [app_model] will always be [None]. *)

val fun_qualid :
  Id_uast.pty Ident.IdTable.t ->
  mod_defs ->
  Parse_uast.qualid ->
  Id_uast.qualid * Ident.t list * Id_uast.pty
(** [fun_qualid ocaml_vals defs q] Searches the Gospel namespace to find the
    function [q]. Also returns the function's type and the type parameters used.
*)

val ocaml_val_qualid :
  mod_defs -> Parse_uast.qualid -> Id_uast.qualid * Id_uast.pty
(** [ocaml_val defs q] Searches the OCaml namespace to find the value [q]. If
    [q] is bound and has a valid Gospel representation, then we return its
    resolved identifier, its OCaml type and its Gospel representation, if it
    exists. *)

val fields_qualid :
  loc:Location.t ->
  mod_defs ->
  Parse_uast.qualid list ->
  (Id_uast.qualid * Id_uast.pty) list * Id_uast.ty_app
(** [fields_qualid defs ~loc l] receives a list of identifiers used to create a
    record and returns:
    - A list of qualified identifiers for each record label in [l] coupled with
      their types.
    - The identifier for the record type the labels [l] belong to.
    - The type parameters for the record type. *)

val get_field_info :
  mod_defs -> Parse_uast.qualid -> Id_uast.qualid * Id_uast.pty * Id_uast.ty_app
(** [get_field_info defs id] receives a record field identifier and returns:
    - The resolved identifier for [q].
    - The type of the record field [q].
    - The identifier for the record type the label [id] belong to.
    - The type parameters for the record type. *)

val get_exn_info :
  mod_defs -> Parse_uast.qualid -> Id_uast.qualid * Id_uast.pty list
(** [get_exn_info defs id] receives an exception identifier [id] and the types
    of its arguments. *)

val gospel_open : env -> Parse_uast.qualid -> Id_uast.qualid * env
(** [gospel_open defs id] adds the definitions in module [id] into the scope
    [defs]. *)

val local_open : mod_defs -> Parse_uast.qualid -> Id_uast.qualid * mod_defs
(** [local_open defs id] is similar to [gospel_open] but is applied to a local
    scope within a term and returns a new local scope with all the definitions
    in module [id]. *)

val empty_env : env
(** The empty environment. The only names in scope are primitive Gospel types.
    This should be the initial environment when processing the Gospel standard
    library. *)

val unit_id : Ident.t
(** The unique identifier for the unit type. *)

val init_env : ?ocamlprimitives:mod_defs -> mod_defs -> env
(** [init_env ocamlprimitives stdlib] receives the definitions in the Gospel
    standard library [stdlib] and creates an environment where every name in
    [stdlib] is in scope as well as every name in [empty_env]. Additionally
    creates a module named [Gospelstdlib] with every definition in [stdlib].
    This should be the initial environment for any Gospel project.

    Another set of definitions [ocamlprimitives] may also be provided which
    contains all primitive OCaml type definitions coupled with their logical
    representations. Besides the one used to type check the Gospel standard
    library, every environment should have these definitions in scope. *)

val submodule : env -> env
(** Returns a new environment for processing a submodule. The variables in scope
    remain the same and the set of definitions is empty. *)
