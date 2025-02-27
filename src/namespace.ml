(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module Env = Map.Make (String)
open Id_uast

type fun_info = { fid : Ident.t }
type ty_info = { tid : Ident.t; tarity : int }

type mod_info = { mid : Ident.t; mdefs : mod_defs }

and mod_defs = {
  fun_env : fun_info Env.t; (* Function definitions *)
  type_env : ty_info Env.t; (* Type definitions *)
  mod_env : mod_info Env.t; (* Nested modules *)
}
(** Set of top level module definitions *)

(** Represents all the definitions within a module *)
let empty_defs =
  { fun_env = Env.empty; type_env = Env.empty; mod_env = Env.empty }

(** [lookup f defs pid] accesses the namespace [f defs] and returns the data
    associated with [pid].
    @raise Not_found
      when there is no identifer with name [pid.pid_str] in the namespace
      associated with [f defs] *)
let lookup f (defs : mod_defs) pid =
  let env = f defs in
  let str = pid.Preid.pid_str in
  Env.find str env

(* Helper functions for field accesses *)

let find_fun = fun d -> d.fun_env
let find_type = fun d -> d.type_env
let find_mod = fun d -> d.mod_env

(** [access_mod defs q] returns a qualified identifer where the identifiers used
    in [q] have been replaced with uniquely tagged identifiers. This function
    assumes that all the identifiers in [q] are belong to the module namespace.
    This function also returns the definitions contained in the module
    associated with [q].
    @raise Not_found
      if any of the identifiers in [q] are not valid module identifiers. TODO,
      This exception should be caught here and subsequently throw a Gospel
      exception. *)
let rec access_mod defs = function
  | Parse_uast.Qid pid ->
      let info = lookup find_mod defs pid in
      let id = info.mid in
      (Qid id, info.mdefs)
  | Qdot (q, pid) ->
      let q, defs = access_mod defs q in
      let info = lookup find_mod defs pid in
      let id = info.mid in
      let defs = info.mdefs in
      (Qdot (q, id), defs)

(** [unique_toplevel f defs q] returns the information associated with the name
    [q] in the environment [f defs]. Additionally, if [q] is of the type
    [M1.M2...Mn.id], returns an optional whose value is the prefix [M1.M2...Mn],
    where every module access has been resolved. Otherwise, returns [None] *)
let unique_toplevel f defs = function
  | Parse_uast.Qid pid ->
      (* If there are no module accesses, we lookup [id] in the
       environment [f defs]. *)
      (None, lookup f defs pid)
  | Qdot (q, pid) ->
      (* If there are module accesses, we first get the environment
       associated with the module represented by [q], and then lookup
       [id] in that environment. *)
      let q, defs = access_mod defs q in
      (Some q, lookup f defs pid)

let mk_qid q id = match q with None -> Qid id | Some q -> Qdot (q, id)

let unique_toplevel_qualid g f defs q =
  let q, info = unique_toplevel f defs q in
  (mk_qid q (g info), info)

let type_info = unique_toplevel_qualid (fun x -> x.tid) find_type
let fun_info = unique_toplevel_qualid (fun x -> x.fid) find_fun
let fun_qualid env q = fst (fun_info env q)

(* Helper functions to add top level definitions into the environment. *)

type env = {
  defs : mod_defs;
  (* Contains the top level definitions in the current module *)
  scope : mod_defs;
      (* Contains the top level definitions currently in scope. A definition is
         in scope if it has been previously defined or exposed through an
         [open]. Note that the [defs] field is not necessarily a subset of
         [scope] since opening modules can shadow previous definitions. *)
}

let defs e = e.defs
let scope e = e.scope
let submodule env = { env with defs = empty_defs }

(** [add_def f env] adds a new definition to the list of definitions and the
    scope. For every function that adds a definition into a [mod_defs] object,
    there should be another function that uses [add_def] to modify the set of
    top level definitions as well as the scope. *)
let add_def f env = { defs = f env.defs; scope = f env.scope }

let add_fun fid defs =
  let env = defs.fun_env in
  let info = { fid } in
  { defs with fun_env = Env.add fid.Ident.id_str info env }

let add_fun env fid = add_def (add_fun fid) env

let add_mod mid mdefs defs =
  let menv = defs.mod_env in
  let info = { mid; mdefs } in
  { defs with mod_env = Env.add mid.Ident.id_str info menv }

let add_mod env mid mdefs = add_def (add_mod mid mdefs) env

let add_type tid tarity defs =
  let tenv = defs.type_env in
  let info = { tid; tarity } in
  { defs with type_env = Env.add tid.Ident.id_str info tenv }

let add_type env tid tarity = add_def (add_type tid tarity) env

(** [type_env] contains every primitive Gospel type. *)
let type_env =
  List.fold_left
    (fun tenv (x, y) -> Env.add x { tid = y; tarity = 0 } tenv)
    Env.empty Structure.primitive_list

(** The empty environment. The only names that it contains with are primitive
    Gospel type definitions. *)
let empty_env = { defs = empty_defs; scope = { empty_defs with type_env } }
