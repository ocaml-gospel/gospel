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
  (* Local term variables e.g. names bound with a [let] or a quantifier *)
  type_vars : (string, Ident.t) Hashtbl.t;
  (* Type variables. *)
  type_nms : Ident.t Env.t;
      (* When processing a recursive type definition, this environment keeps
         track of all the type names that are not aliases that the definition
         introduces. *)
}
(** Keeps track of the variables within the scope of a term. We use an immutable
    environment for term variables since it makes it easier to manage scopes. We
    use a mutable table for type variables since there is no notion of scopes
    for type variables: any type variable that appears in a Gospel term is
    assumed to be defined at the top of the Gospel structure in a universal
    quantifier. *)

let empty_local_env () =
  { term_var = Env.empty; type_vars = Hashtbl.create 100; type_nms = Env.empty }

(** [add_term_var var id env] maps the term variable [var] to the tagged
    identifier [id] *)
let add_term_var var id env =
  { env with term_var = Env.add var id env.term_var }

(** [add_type_var var id env] maps the type variable [var] to the tagged
    identifier [id] *)
let add_type_var id env = Hashtbl.add env.type_vars id.Ident.id_str id

let add_type_nm id env =
  { env with type_nms = Env.add id.Ident.id_str id env.type_nms }

(** [type_vars ~bind env pty] returns a list of all the type variables used in
    [pty]. If the [bind] flag is false, then this type is not allowed to
    introduce type variables that are not already defined in [env]. *)
let type_vars ~bind env pty =
  let rec type_vars = function
    | Parse_uast.PTtyvar v when Hashtbl.mem env.type_vars v.Preid.pid_str ->
        (* When a type variable is already in scope, we do not need to do
          anything. *)
        ()
    | PTtyvar v ->
        (* This case is reached when the type variable [v] is not in scope. *)
        if bind then
          (* If we are allowed to add type variables to the scope, we add it to
             the environment. *)
          let id = Ident.from_preid v in
          add_type_var id env
        else
          (* This case is only reached when the variable [pid] is unbound and we
            cannot add new type variables to the environment. *)
          W.error ~loc:v.pid_loc (W.Unbound_type_variable v.pid_str)
    | PTtyapp (_, l) | PTtuple l -> List.iter type_vars l
    | PTarrow (t1, t2) ->
        type_vars t1;
        type_vars t2
  in
  type_vars pty

(** [unique_pty ~bind defs env pty vars] Returns a type annotation where each
    type symbol (including type variables) has been replaced with a unique
    identifier. Type variables are searched in [env] and type names are searched
    in [defs]. When we are processing type definitions, type names are also be
    searched for in [env]. *)
let unique_pty ~bind defs env pty =
  let rec unique_pty = function
    | Parse_uast.PTtyvar pid -> PTtyvar (Hashtbl.find env.type_vars pid.pid_str)
    | PTtyapp (Qid id, l) when Env.mem id.pid_str env.type_nms ->
        (* This branch is reached when we are processing a set of recursive type
          definitions and [id] is one of the type names. *)
        let info = Types.mk_info (Qid (Env.find id.pid_str env.type_nms)) in
        PTtyapp (info, List.map unique_pty l)
    | PTtyapp (q, l) ->
        (* When we encounter a type identifier, we must check if it is an alias
           for another type and if so replace it. *)
        resolve_alias defs q (List.map unique_pty l)
    | PTarrow (arg, res) ->
        let arg = unique_pty arg in
        let res = unique_pty res in
        PTarrow (arg, res)
    | PTtuple l -> PTtuple (List.map unique_pty l)
  in
  let () = type_vars ~bind env pty in
  unique_pty pty

(** [unique_var env defs q] returns a qualified identifer where the identifiers
    used in [q] have been replaced with uniquely tagged identifiers. If [q] has
    no module accesses (i.e. is not of the form id1.id2) then this function
    returns the binding in [env] if it exists. Otherwise it uses [defs] to
    search for the tagged identifiers. *)
let unique_var env defs q =
  match q with
  | Parse_uast.Qid pid when Env.mem pid.pid_str env ->
      Tlocal (Env.find pid.pid_str env)
  | _ ->
      (* If [q] is of the form [Qdot] it cannot be a local variable *)
      let q, params, id = fun_qualid defs q in
      Tvar (q, params, id)

(** [binder (pid, pty)] maps the string [pid.pid_str] to a fresh tagged
    variable, adds all the type variables present in [pty] to the local scope
    and turns [pty] into a tagged annotation. *)
let binder defs env (pid, pty) =
  let id = Ident.from_preid pid in
  env := add_term_var pid.pid_str id !env;
  let pty = Option.map (unique_pty ~bind:true defs !env) pty in
  (id, pty)

(** [unique_term top defs env t] returns a term where every variable in [t] has
    been replaced with a uniquely tagged variable. When we find a free variable,
    we first search the local scope. If it is not found then we search the top
    level. When a variable from the top level is used, we must add its typing
    information to the AST. This is necessary for the type inference phase so
    that the Inferno solver knows the type of all the variables outside the
    scope of the term. When a variable is bound either with a [let] or a
    quantifier, we create a new unique identifier and map it in [env]. *)
let rec unique_term defs env t =
  (* To be used when we perform a local open. *)
  let unique_term_open = unique_term in
  (* The namespace remains constant in all other recursive calls *)
  let unique_term = unique_term defs in
  let term_desc =
    match t.Parse_uast.term_desc with
    | Parse_uast.Ttrue -> Ttrue
    | Tfalse -> Tfalse
    | TTrue -> TTrue
    | TFalse -> TFalse
    | Tconst c -> Tconst c
    | Tvar q -> unique_var env.term_var defs q
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
        let l = List.map (binder defs env) l in
        let t = unique_term !env t in
        Tquant (q, l, t)
    | Tif (g, then_b, else_b) ->
        let g = unique_term env g in
        let then_b = unique_term env then_b in
        let else_b = unique_term env else_b in
        Tif (g, then_b, else_b)
    | Ttuple l -> Ttuple (List.map (unique_term env) l)
    | Tlambda (args, t, pty) ->
        let env = ref env in
        let pty = Option.map (unique_pty ~bind:true defs !env) pty in
        let args = List.map (binder defs env) args in
        let t = unique_term !env t in
        Tlambda (args, t, pty)
    | Trecord l ->
        let qids, info =
          Namespace.fields_qualid ~loc:t.term_loc defs (List.map fst l)
        in
        let values =
          List.map2 (fun (id, ty) (_, t) -> (id, unique_term env t, ty)) qids l
        in
        Trecord (values, info)
    | Tfield (t, q) ->
        let qid, fty, record_ty = Namespace.get_field_info defs q in
        Tfield (unique_term env t, record_ty, qid, fty)
    | Tattr (att, t) -> Tattr (att, unique_term env t)
    | Tcast (t, pty) ->
        let ty = unique_pty ~bind:true defs env pty in
        let t = unique_term env t in
        Tcast (t, ty)
    | Tscope (q, t) ->
        let q, defs = Namespace.local_open defs q in
        let t = unique_term_open defs env t in
        Tscope (q, t)
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

(** [get_tvars env] returns the list of type variables defined in [env]. *)
let get_tvars env = Hashtbl.to_seq_values env.type_vars |> List.of_seq

let fun_spec defs env spec =
  let unique_term = unique_term defs env in
  {
    fun_req = List.map unique_term spec.Parse_uast.fun_req;
    fun_ens = List.map unique_term spec.fun_ens;
    fun_variant = List.map unique_term spec.fun_variant;
    fun_text = spec.fun_text;
    fun_loc = spec.fun_loc;
  }

let function_ f defs =
  let fun_name = Ident.from_preid f.Parse_uast.fun_name in
  let fun_rec = f.fun_rec in
  let env = ref (empty_local_env ()) in
  let () = param_dups f.fun_params in
  let fun_type = Option.map (unique_pty ~bind:true defs !env) f.fun_type in
  let fun_params =
    List.map
      (fun (pid, pty) ->
        let id = Ident.from_preid pid in
        (* Add the function parameters to the local environment. *)
        env := add_term_var pid.pid_str id !env;
        (* Add the type variables in [pty] to the local environment. *)
        let pty = unique_pty ~bind:true defs !env pty in
        (id, pty))
      f.fun_params
  in
  let fun_spec = fun_spec defs !env f.fun_spec in
  let () =
    if fun_rec then
      (* If the function is recursive, add it as a local variable. *)
      env := add_term_var fun_name.id_str fun_name !env
    else ()
  in
  let fun_def = Option.map (unique_term defs !env) f.fun_def in
  let vars = get_tvars !env in
  let fun_loc = f.fun_loc in
  let f =
    { fun_name; fun_rec; fun_type; fun_params; fun_def; fun_spec; fun_loc }
  in
  (f, vars)

let axiom defs ax =
  let ax_name = Ident.from_preid ax.Parse_uast.ax_name in
  let local_env = empty_local_env () in
  let ax_term = unique_term defs local_env ax.ax_term in
  let ax_loc = ax.ax_loc in
  let ax_text = ax.ax_text in
  ({ ax_name; ax_term; ax_loc; ax_text }, get_tvars local_env)

(** [tdecl_env l] creates a local environment that contains each type name
    defined in [l] that is not a type alias. *)
let tdecl_env (l : Parse_uast.s_type_declaration list) =
  let open Parse_uast in
  let add_decl env t =
    (* Check if [t] is a type alias. *)
    if Option.is_none t.tmanifest then
      let id = Ident.from_preid t.tname in
      add_type_nm id env
    else env
  in
  List.fold_left add_decl (empty_local_env ()) l

(** [get_alias_deps aliases pty] returns a list consisting of every type name in
    [aliases] that [pty] uses. Naturally, the returned list must be a sub list
    of [aliases]. *)
let get_alias_deps aliases pty =
  let deps = ref [] in
  (* Checks if an identifier is in the [aliases] list and has not already been
     added to the [deps] list. *)
  let is_alias = function
    | Parse_uast.Qid id ->
        if List.mem id.pid_str aliases && not (List.mem id.pid_str !deps) then
          deps := id.pid_str :: !deps
    | _ -> ()
  in
  (* Iterator function that updates [deps] in place. *)
  let rec loop = function
    | Parse_uast.PTtyvar _ -> ()
    | PTtyapp (q, l) ->
        is_alias q;
        List.iter loop l
    | PTarrow (arg, res) ->
        loop arg;
        loop res
    | PTtuple l -> List.iter loop l
  in
  loop pty;
  !deps

(** [alias_order l] returns a list consisting of the names of the types aliases
    defined in [l]. Moreover, the names appear in the order in which the type
    aliases should be added to the namespace so that any type aliases that
    depend on other type aliases can be expanded. Example:

    [type t3 = {x : t2} and  t2 = t1 and t1 = t3]

    When we add [t3] to the namespace, we want to expand the record type
    annotation [x : t2] to [x : t3], however, that can only be done after we
    process the type aliases [t1] and [t2]. Moreover, [t1] must be processed
    before [t2], since the definition of [t2] uses [t1]. Given this, the
    resulting list will be [[t1; t2]]. Note that [t3] is not included seeing as
    it is not a type alias. *)
let alias_order l =
  let open Parse_uast in
  (* List with each type name that is an alias for some other type. *)
  let aliases =
    List.filter_map
      (fun decl ->
        if Option.is_some decl.tmanifest then
          Some (decl.tname.pid_str, decl.tloc)
        else None)
      l
  in
  let names = List.map fst aliases in
  (* [node decl] returns [Some (nm, deps)] where [nm] is the name of the type
     and [deps] are the aliases that [decl] uses in its definition. *)
  let node decl =
    Option.map
      (fun pty -> (decl.tname.pid_str, get_alias_deps names pty))
      decl.tmanifest
  in
  let graph = List.filter_map node l in
  try Utils.depends graph
  with Utils.Cycle (x, l) ->
    let loc = List.assoc x aliases in
    let error =
      match l with
      | [] -> W.Cyclic_definition x
      | _ -> W.Cyclic_definitions (x, l)
    in
    W.error ~loc error

let type_kind env tid tparams lenv = function
  | Parse_uast.PTtype_abstract -> (env, PTtype_abstract)
  | PTtype_record l ->
      (* Function to create a unique record field *)
      let field (id, pty) =
        let id = Ident.from_preid id in
        let pty = unique_pty ~bind:false (scope env) lenv pty in
        (id, pty)
      in
      let fields = List.map field l in
      (* If this is an OCaml record declaration, the record fields are not
         inserted into the namespace. *)
      let env = add_record env tid tparams fields in
      (env, PTtype_record fields)

let unique_tspec env local_env self_ty tspec =
  (* [invariants [pid, l] processes a list of invariants for the give type
     where [pid] is a variable of type [self_ty]. *)
  let invariants (pid, l) =
    let id = Ident.from_preid pid in
    let inv t =
      let local_env = add_term_var id.id_str id local_env in
      let t = unique_term env local_env t in
      let tvars = get_tvars local_env in
      Solver.invariant tvars id self_ty t
    in
    (id, List.map inv l)
  in
  Tast.mk_type_spec tspec.Parse_uast.ty_ephemeral
    (Option.map invariants tspec.ty_invariant)
    tspec.ty_text tspec.ty_loc

(** [ghost_type_decl lenv env t] processes the ghost type declaration [t] and
    adds its definitions to [env], producing a new environment. Additionally,
    this function also produces a closure which, when given an environment, will
    generate a typed declaration. The reason we do not generate it immediately
    is because the type specification of [t] may refer to record fields that
    have not been introduced into the scope yet. *)
let ghost_type_decl lenv env t =
  let defs = scope env in
  (* If the identifier for this type has already been generated and added to the
     environment, we use it. If not (either this is a type alias or this is a
     non-recursive type definition) we generate a new identifier. *)
  let tname =
    try Env.find t.Parse_uast.tname.pid_str lenv.type_nms
    with Not_found -> Ident.from_preid t.tname
  in
  let () =
    (* Raise an error if two type parameters have the same name *)
    let error x =
      W.error ~loc:x.Preid.pid_loc (W.Duplicated_parameter x.Preid.pid_str)
    in
    Utils.duplicate Preid.eq error t.tparams
  in
  let tparams = List.map Ident.from_preid t.tparams in
  (* Keep the local definitions within [lenv] but refresh the type variables
     table so that it is independent of the other type definitions we are processing *)
  let tvar_env = { lenv with type_vars = Hashtbl.create 100 } in
  let () = List.iter (fun param -> add_type_var param tvar_env) tparams in
  (* We call [unique_pty] with the [bind] flag to false so that an error is
     raised if any type variables besides those defined in [tparams] are used. *)
  let tmanifest =
    Option.map (unique_pty ~bind:false defs tvar_env) t.tmanifest
  in

  let env, tkind = type_kind env tname tparams tvar_env t.tkind in

  (* The [pty] object that represents this type name. This is necessary for the
     typed specification, where there will always be a variable of this type.*)
  let info = Types.mk_info ~alias:tmanifest (Qid tname) in
  let self_ty = PTtyapp (info, List.map (fun x -> PTtyvar x) tparams) in

  (* Closure that generates the typed declaration. Note that all this closure
     does is process the type specification. *)
  let def_gen =
   fun env ->
    let tspec =
      Option.fold ~none:Tast.empty_tspec
        ~some:(unique_tspec (scope env) tvar_env self_ty)
        t.tspec
    in
    Tast.mk_tdecl tname tparams tkind t.tprivate tmanifest t.tattributes tspec
      t.tloc
  in
  let env = add_type env tname tparams tmanifest in
  (env, def_gen)

(** [tdecl_duplicates l] Raises an exception if there are duplicate type names
    or duplicate record fields in a list of type definitions. If there are none,
    this function has no observable effect. *)
let tdecl_duplicates l =
  let open Parse_uast in
  let open Preid in
  (* Check if there are two type definitions with the same name *)
  let eq = fun x y -> Preid.eq x.tname y.tname in
  let t_error d =
    raise (W.error ~loc:d.tloc (W.Duplicated_type_definition d.tname.pid_str))
  in

  (* Check if there are two record fields with the same name across all type definitions *)
  let get_fields = function PTtype_record l -> List.map fst l | _ -> [] in
  let all_fields = List.concat_map (fun decl -> get_fields decl.tkind) l in
  let field_error l =
    raise (W.error ~loc:l.pid_loc (W.Duplicated_record_label l.pid_str))
  in
  Utils.duplicate Preid.eq field_error all_fields;
  Utils.duplicate eq t_error l

(** [ghost_tdecl_list env l] processes a list of mutually recursive type
    definitions. It does this by:

    - Checking if there are duplicate type names or record labels.

    - Creating a local environment which track the non-aliased type names
      introduced by [l].

    - Splitting the type declarations into aliases and non-aliases and placing
      the aliases in the correct order (to see what exactly that means, see the
      documentation for [alias_order].)

    - Processing and adding each alias to the namespace.

    - Adding the remaining type definitions to the namespace. In this step and
      the previous, if a type uses one of the aliases defined in [l], these are
      fetched from the namespace whereas the definitions for non-aliased
      definitions are fetched from the local environment.

    - Once we have processed each type definition, we put them back in the order
      in which they appear in [l]. This done only so that the original AST and
      the typed AST are as similar as possible. *)
let ghost_tdecl_list env l =
  let open Parse_uast in
  (* Create a local environment with the non-aliased type names introduced by
     [l]. *)
  let () = tdecl_duplicates l in
  let lenv = tdecl_env l in

  (* List with the aliases in the order in which they must be processed. *)
  let order = alias_order l in
  let aliases_order =
    List.map (fun x -> List.find (fun decl -> decl.tname.pid_str = x) l) order
  in
  (* List with every type definition in [l] with the type aliases at the top in
     the correct order. *)
  let all_defs =
    aliases_order
    @ List.filter (fun x -> not (List.mem x.tname.pid_str order)) l
  in

  (* Process each definition and update the environment accordingly. *)
  let env, defs =
    List.fold_left
      (fun (env, defs) def ->
        let env, def = ghost_type_decl lenv env def in
        (env, def :: defs))
      (env, []) all_defs
  in
  (* Now that every record field has been added the scope, we can produce the
     typed declaration *)
  let defs = List.map (fun gen -> gen env) defs in

  (* Place the type definitions in the order the user put them. *)
  let defs_og_order =
    List.map
      (fun pdecl ->
        List.find
          (fun id_decl -> pdecl.tname.pid_str = id_decl.Tast.tname.id_str)
          defs)
      l
  in
  (env, defs_og_order)

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
    | Sig_attribute att -> (Sig_attribute att, env)
    | _ -> assert false
  in
  ({ Tast.sdesc; sloc = s.sloc }, env)

and gospel_sig env = function
  | Parse_uast.Sig_function f ->
      let f, vars = function_ f (scope env) in
      let f, fun_ty = Solver.function_ vars f in
      let env = add_fun env f.fun_name fun_ty in
      (Tast.Sig_function f, env)
  | Sig_axiom ax ->
      (* Since axioms cannot be referenced, the environment is not
        modified.*)
      let ax, vars = axiom (scope env) ax in
      let ax = Solver.axiom vars ax in
      (Sig_axiom ax, env)
  | Sig_ghost_type t ->
      let env, t = ghost_tdecl_list env t in
      (Sig_ghost_type t, env)
  | Sig_ghost_open q ->
      let q, env = Namespace.gospel_open env q in
      (Sig_ghost_open q, env)

(** [signatures l env] Processes a list of top level signatures along with the
    current environment. *)
and signatures l env =
  match l with
  | [] -> ([], env)
  | s :: t ->
      let s, env = signature s env in
      let t, env = signatures t env in
      (s :: t, env)

let signatures env l =
  let l, env = signatures l env in
  (l, Namespace.defs env)
