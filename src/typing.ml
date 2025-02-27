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
module W = Warnings
open Namespace

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
  let id = Ident.to_local id in
  { env with term_var = Env.add var id env.term_var }

(** [add_type_var var id env] maps the type variable [var] to the tagged
    identifier [id] *)
let add_type_var var id env =
  { env with type_var = Env.add var id env.type_var }

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
        let q, tinfo = type_info defs q in
        let id = tinfo.tid in
        let expected = tinfo.tarity in
        let arity = List.length l in
        if arity <> expected then
          W.error ~loc:id.id_loc (W.Bad_arity (id.id_str, expected, arity));
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
      fun_qualid defs q

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
      (* If the function is recursive, add it as a local variable. *)
      env := add_term_var fun_name.id_str fun_name !env
    else ()
  in
  let fun_def = Option.map (unique_term defs !env) f.fun_def in
  let fun_spec =
    ignore f.fun_spec;
    None
  in
  let fun_loc = f.fun_loc in
  { fun_name; fun_rec; fun_type; fun_params; fun_def; fun_spec; fun_loc }

let axiom defs ax =
  let ax_name = Ident.from_preid ax.Parse_uast.ax_name in
  let ax_term = unique_term defs empty_local_env ax.ax_term in
  let ax_loc = ax.ax_loc in
  let ax_text = ax.ax_text in
  { ax_name; ax_term; ax_loc; ax_text }

let type_kind = function Parse_uast.PTtype_abstract -> PTtype_abstract

let ghost_type_decl t =
  let tname = Ident.from_preid t.Parse_uast.tname in
  let () =
    (* Raise an error if two type parameters have the same name *)
    let error x =
      W.error ~loc:x.Preid.pid_loc (W.Duplicated_parameter x.Preid.pid_str)
    in
    Utils.duplicate Preid.eq error t.tparams
  in
  let tparams = List.map Ident.from_preid t.tparams in
  {
    tname;
    tparams;
    tprivate = t.tprivate;
    tkind = type_kind t.tkind;
    tattributes = t.tattributes;
    tspec = None;
    tloc = t.tloc;
  }

let rec process_module env m =
  (* TODO figure out when this is None *)
  let id = Option.map Ident.from_preid m.Parse_uast.mdname in
  (* Since we are now at the beginning of a new module, we create an environment
     with the same variables in scope, but with no module definitions. *)
  let sub_mod_env = submodule env in
  let mdesc, mod_defs =
    match m.mdtype.mdesc with
    | Mod_signature m ->
        (* The returned environment contains the all the definitions in this
           submodule. *)
        let s, env = signatures m sub_mod_env in
        (* We ignore the [scope] field in [env] as it is not relevant after
           processing the module. *)
        (Tast.Mod_signature s, defs env)
    | _ -> assert false
  in
  (* If this module has an identifier, then this function adds it to the current
     definitions *)
  let env =
    Option.fold ~none:env ~some:(fun id -> add_mod env id mod_defs) id
  in
  (* Rebuild the module object *)
  let mloc = m.mdtype.mloc in
  let mattributes = m.mdtype.mattributes in
  let mdtype = { Tast.mdesc; mloc; mattributes } in
  let s =
    Tast.Sig_module
      { mdname = id; mdtype; mdattributes = m.mdattributes; mdloc = m.mdloc }
  in
  (s, env)

(** [signature s env] processes [s] and returns a new environment where the
    names in [s] have been added to [env]. *)
and signature s env =
  let sdesc, env =
    match s.Parse_uast.sdesc with
    | Sig_gospel (s, _) -> gospel_sig env s
    | Sig_module m -> process_module env m
    | _ -> assert false
  in
  ({ Tast.sdesc; sloc = s.sloc }, env)

and gospel_sig env = function
  | Parse_uast.Sig_function f ->
      let f = function_ f (scope env) in
      let f = Solver.function_ f in
      let env = add_fun env f.fun_name in
      (Tast.Sig_function f, env)
  | Sig_axiom ax ->
      (* Since axioms cannot be referenced, the environment is not
        modified.*)
      let ax = axiom (scope env) ax in
      let ax = Solver.axiom ax in
      (Sig_axiom ax, env)
  | Sig_ghost_type t ->
      let t = ghost_type_decl t in
      let arity = List.length t.tparams in
      let env = add_type env t.tname arity in
      (Sig_ghost_type t, env)
  | _ -> assert false

(** [signatures l env] Processes a list of top level signatures along with the
    current environment. *)
and signatures (l : Parse_uast.s_signature) env =
  match l with
  | [] -> ([], env)
  | s :: t ->
      let s, env = signature s env in
      let t, env = signatures t env in
      (s :: t, env)

let signatures l = fst (signatures l empty_env)
