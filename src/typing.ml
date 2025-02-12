(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Id_uast
module Env = Map.Make (String)
module W = Warnings

type fun_info = { fid : Ident.t }
type ty_info = { tid : Ident.t }

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

(* Helper functions to add top level definitions into the environment. *)

let add_fun fid defs =
  let env = defs.fun_env in
  let info = { fid } in
  { defs with fun_env = Env.add fid.Ident.id_str info env }

let add_mod mid mdefs defs =
  let menv = defs.mod_env in
  let info = { mid; mdefs } in
  { defs with mod_env = Env.add mid.Ident.id_str info menv }

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

(** [unique_toplevel_qualid f defs q] returns the information associated with
    the name [q]. Additionally, if [q] is of the type [M1.M2...Mn.id], returns
    an optional whose value is the prefix [M1.M2...Mn], where every module
    access has been resolved. Otherwise, returns [None] *)
let unique_toplevel_qualid f defs = function
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

type local_env = {
  term_var : Ident.t Env.t;
  (* Local term variables bound with a [let] or a quantifier *)
  type_var : Ident.t Env.t; (* Type variables. *)
}
(** Keeps track of the variables within the scope of a term. *)

let empty_local_env = { term_var = Env.empty; type_var = Env.empty }

(** [add_term_var var id env] maps the term variable [var] to the tagged
    identifier [id] *)
let add_term_var var id env =
  { env with term_var = Env.add var id env.term_var }

(** [add_type_var var id env] maps the type variable [var] to the tagged
    identifier [id] *)
let add_type_var var id env =
  { env with type_var = Env.add var id env.type_var }

let mk_qid q id = match q with None -> Qid id | Some q -> Qdot (q, id)

(** [unique_pty defs env pty] Returns a type annotation where each type symbol
    (including type variables) has been replaced with a unique identifier. Type
    variables are searched in [env] and type names are search in [defs]. Type
    annotations are allowed to introduce type variables into the local scope but
    are not allowed to introduce fresh type names. When new type variables are
    introduced, the [env] is modified in place. *)
let unique_pty defs env =
  let rec unique_pty = function
    | Parse_uast.PTtyvar pid when Env.mem pid.pid_str !env.type_var ->
        PTtyvar (Env.find pid.pid_str !env.type_var)
    | PTtyvar pid ->
        let id = Ident.from_preid pid in
        env := add_type_var pid.pid_str id !env;
        PTtyvar id
    | PTtyapp (q, l) ->
        let q, info = unique_toplevel_qualid find_type defs q in
        let q = mk_qid q info.tid in
        PTtyapp (q, List.map unique_pty l)
    | PTtuple l -> PTtuple (List.map unique_pty l)
    | PTarrow (arg, res) ->
        let arg = unique_pty arg in
        let res = unique_pty res in
        PTarrow (arg, res)
  in
  unique_pty

(** [unique_var env defs q] returns a qualified identifer where the identifiers
    used in [q] have been replaced with uniquely tagged identifiers. If [q] has
    no module accesses (i.e. is not of the form id1.id2) then this function
    returns the binding in [env] if it exists. Otherwise it uses [defs] to
    search for the tagged identifiers. *)
let unique_var env defs q =
  match q with
  | Parse_uast.Qid pid when Env.mem pid.pid_str env ->
      Id_uast.Qid (Env.find pid.pid_str env)
  | _ ->
      (* If [q] is of the form [Qdot] it cannot be a local variable *)
      let q, info = unique_toplevel_qualid find_fun defs q in
      mk_qid q info.fid

(** [unique_term defs env t] returns a term where every variable in [t] has been
    replaced with a uniquely tagged variable. When we find a free variable, we
    first search the local scope. If it is not found then we search the top
    level. When a variable is bound either with a [let] or a quantifier, we
    create a new unique identifier and map it to [env]. *)
let rec unique_term defs env t =
  (* The namespace remains constant in each recursive call *)
  let unique_term = unique_term defs in
  let term_desc =
    match t.Parse_uast.term_desc with
    | Parse_uast.Ttrue -> Ttrue
    | Tfalse -> Tfalse
    | Tconst c -> Tconst c
    | Tvar q -> Tvar (unique_var env.term_var defs q)
    | Tlet (v, t1, t2) ->
        let id = Ident.from_preid v in
        let t1 = unique_term env t1 in
        (* Add the identifier to the local environment *)
        let t2 = unique_term (add_term_var v.pid_str id env) t2 in
        Tlet (id, t1, t2)
    | Tapply (t1, t2) -> Tapply (unique_term env t1, unique_term env t2)
    | Tinfix _ -> (unique_term env (Uast_utils.chain t)).term_desc
    | Tquant (q, l, t) ->
        let env = ref env in
        (* [binder (pid, pty)] maps the string [pid.pid_str] to a
           fresh tagged variable, adds all the type variables present
           in [pty] to the local scope and turns [pty] into a tagged
           annotation. *)
        let binder (pid, pty) =
          let id = Ident.from_preid pid in
          env := add_term_var pid.pid_str id !env;
          let pty = Option.map (unique_pty defs env) pty in
          (id, pty)
        in
        let l = List.map binder l in
        let t = unique_term !env t in
        Tquant (q, l, t)
    | Tif (g, then_b, else_b) ->
        let g = unique_term env g in
        let then_b = unique_term env then_b in
        let else_b = unique_term env else_b in
        Tif (g, then_b, else_b)
    | _ -> assert false
  in
  { term_desc; term_loc = t.term_loc }

(* Helper functions for top level signatures *)

(** [param_dups l] checks if there are any duplicate function parameters in [l].
    If so, this function throws an appropriate Gospel exception. If not, this
    function has no effect *)
let param_dups l =
  let open Preid in
  let error (x, _) = W.error ~loc:x.pid_loc (W.Duplicated_argument x.pid_str) in
  Utils.duplicate (fun (x, _) (y, _) -> eq x y) error l

let function_ f defs =
  let fun_name = Ident.from_preid f.Parse_uast.fun_name in
  let fun_rec = f.fun_rec in
  let env = ref empty_local_env in
  let () = param_dups f.fun_params in
  let fun_type = Option.map (unique_pty defs env) f.fun_type in
  let fun_params =
    List.map
      (fun (pid, pty) ->
        let id = Ident.from_preid pid in
        (* Add the function parameters to the local environment. *)
        env := add_term_var pid.pid_str id !env;
        (* Add the type variables in [pty] to the local environment. *)
        let pty = unique_pty defs env pty in
        (id, pty))
      f.fun_params
  in
  let () =
    if fun_rec then
      (* If the function is recursive, add it as a local variable: it
         cannot be handled as a top level definition (i.e. a
         definition within [defs]) because we need Inferno to infer
         its type. For more details, please refer to the comment in
         [Checker.fun_types]*)
      env := add_term_var fun_name.id_str fun_name !env
    else ()
  in
  let fun_def = Option.map (unique_term defs !env) f.fun_def in
  let fun_spec = Option.map (fun _ -> assert false) f.fun_spec in
  let fun_loc = f.fun_loc in
  let fun_text = f.fun_text in
  {
    fun_name;
    fun_rec;
    fun_type;
    fun_params;
    fun_def;
    fun_spec;
    fun_loc;
    fun_text;
  }

let axiom defs ax =
  let ax_name = Ident.from_preid ax.Parse_uast.ax_name in
  let ax_term = unique_term defs empty_local_env ax.ax_term in
  let ax_loc = ax.ax_loc in
  let ax_text = ax.ax_text in
  { ax_name; ax_term; ax_loc; ax_text }

type env = {
  defs : mod_defs;
  (* Contains the top level definitions in the current module *)
  scope : mod_defs;
      (* Contains the top level definitions currently in scope. A definition is in
     scope if it has been previously defined or exposed through an [open]. Not
     necessarily a subset of [defs_mod], since opening modules can shadow
     previous definitions. *)
}
(** Environment for processing top level signatures. *)

(** [add_def f env] applies the function [f] to the list of module definitions
    and to the scope of the environment. This is useful when adding new
    definitions to a module, since these have to also be added to the current
    scope. *)
let add_def f env = { defs = f env.defs; scope = f env.scope }

let rec process_module env m =
  (* TODO figure out when this is None *)
  let id = Option.map Ident.from_preid m.Parse_uast.mdname in
  (* Since we are now at the beginning of a new module, we create an environment
     with the same variables in scope, but with no module definitions. *)
  let sub_mod_env = { env with defs = empty_defs } in
  let mdesc, mod_defs =
    match m.mdtype.mdesc with
    | Mod_signature m ->
        (* The returned environment contains the all the definitions in this
           submodule. *)
        let s, env = signatures m sub_mod_env in
        (* We ignore the [scope] field in [env] as it is not relevant after
           processing the module. *)
        (Mod_signature s, env.defs)
    | _ -> assert false
  in
  (* If this module has an identifier, then this function adds it to the current
     definitions *)
  let f defs =
    Option.fold ~none:defs ~some:(fun id -> add_mod id mod_defs defs) id
  in
  (* Rebuild the module object *)
  let env = add_def f env in
  let mloc = m.mdtype.mloc in
  let mattributes = m.mdtype.mattributes in
  let mdtype = { mdesc; mloc; mattributes } in
  let s =
    Sig_module
      { mdname = id; mdtype; mdattributes = m.mdattributes; mdloc = m.mdloc }
  in
  (s, env)

(** [signature s] processes [f] and changes the global variables [defs_mod] and
    [defs_scope] whenever a new name is added to the top level. *)
and signature s env =
  let sdesc, env =
    match s.Parse_uast.sdesc with
    | Parse_uast.Sig_function f ->
        let f = function_ f env.scope in
        (* Adds [f] to the set of variables defined and to the
          scope *)
        let env = add_def (add_fun f.fun_name) env in
        (Sig_function f, env)
    | Sig_axiom ax ->
        (* Since axioms cannot be referenced, the environment is not
          modified.*)
        let ax = axiom env.scope ax in
        (Sig_axiom ax, env)
    | Sig_module m -> process_module env m
    | _ -> assert false
  in
  ({ sdesc; sloc = s.sloc }, env)

(** [signatures l env] Processes a list of top level signatures along with the
    current environment. *)
and signatures (l : Parse_uast.s_signature) env =
  match l with
  | [] -> ([], env)
  | s :: t ->
      let s, env = signature s env in
      let t, env = signatures t env in
      (s :: t, env)

(** An environment with primitive type definitions. *)
let type_env =
  List.fold_left
    (fun tenv (x, y) -> Env.add x { tid = y } tenv)
    Env.empty Structure.primitive_list

(** The empty environment *)
let empty_env = { defs = empty_defs; scope = { empty_defs with type_env } }

let signatures l = fst (signatures l empty_env)
