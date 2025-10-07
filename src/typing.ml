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
  ocaml_vals : Id_uast.pty Ident.IdTable.t;
  (* When processing an OCaml value description, this table keeps
     track of the OCaml values that are allowed to be used within
     Gospel terms. *)
  type_vars : (string, Ident.t) Hashtbl.t;
  (* Type variables. *)
  type_nms : Ident.t Env.t;
  (* When processing a recursive type definition, this environment keeps
     track of all the type names that are not aliases that the definition
     introduces. *)
  ocaml_type_nms : Ident.t Env.t;
      (* Same as the previous field, but for OCaml type names. *)
}
(** Keeps track of the variables within the scope of a term. We use an immutable
    environment for term variables since it makes it easier to manage scopes. We
    use a mutable table for type variables since there is no notion of scopes
    for type variables: any type variable that appears in a Gospel term is
    assumed to be defined at the top of the Gospel structure in a universal
    quantifier. *)

let empty_local_env () =
  {
    term_var = Env.empty;
    type_vars = Hashtbl.create 100;
    ocaml_vals = Ident.IdTable.create 100;
    type_nms = Env.empty;
    ocaml_type_nms = Env.empty;
  }

(** [add_term_var var id env] maps the term variable [var] to the tagged
    identifier [id] in [env] and [old_env] if it is different from [None]. *)
let add_spec_var id old_env env =
  let old_env =
    Option.map
      (fun old_env ->
        { old_env with term_var = Env.add id.Ident.id_str id old_env.term_var })
      old_env
  in
  (old_env, { env with term_var = Env.add id.id_str id env.term_var })

let add_term_var id env = snd (add_spec_var id None env)

(** [add_type_var var id env] maps the type variable [var] to the tagged
    identifier [id] *)
let add_type_var id env = Hashtbl.add env.type_vars id.Ident.id_str id

let add_type_nm ~ocaml id env =
  if ocaml then
    { env with ocaml_type_nms = Env.add id.Ident.id_str id env.ocaml_type_nms }
  else { env with type_nms = Env.add id.Ident.id_str id env.type_nms }

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

let is_local_type ~ocaml id env =
  let env = if ocaml then env.ocaml_type_nms else env.type_nms in
  Env.mem id.Preid.pid_str env

let get_local_type ~ocaml id env =
  let env = if ocaml then env.ocaml_type_nms else env.type_nms in
  Env.find id.Preid.pid_str env

(** [unique_pty ~ocaml ~bind defs env pty vars] Returns a type annotation where
    each type symbol (including type variables) has been replaced with a unique
    identifier. Type variables are searched in [env] and type names are searched
    in [defs]. When we are processing type definitions, type names are also be
    searched for in [env].

    If the [bind] flag is [false] then this function is not allowed to introduce
    any type variables that are not already defined in [env]. The [ocaml] flag
    is used to determine if we will search for type names in the [ocaml]
    namespace or the [gospel] namespace. *)
let unique_pty ~ocaml ~bind defs env pty =
  let rec unique_pty = function
    | Parse_uast.PTtyvar pid -> PTtyvar (Hashtbl.find env.type_vars pid.pid_str)
    | PTtyapp (Qid id, l) when is_local_type ~ocaml id env ->
        (* This branch is reached when we are processing a set of recursive type
          definitions and [id] is one of the type names. *)
        let info = Types.mk_info (Qid (get_local_type ~ocaml id env)) in
        PTtyapp (info, List.map unique_pty l)
    | PTtyapp (q, l) ->
        let l = List.map unique_pty l in
        let app = resolve_application ~ocaml defs q l in
        PTtyapp (app, l)
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
    no module accesses (i.e. is not of the form M.id) then this function returns
    the binding in [env] if it exists (this is the case if the variable is bound
    locally within the term.).

    If [q] is not in [env] or is a qualified variable, we first search in the
    OCaml namespace and then check if the variable is allowed to be used within
    the term we are processing (i.e. this variable appears in an ownership
    clause of the specification this term is in). If the variable is either not
    an OCaml variable or not allowed to be used in this term, we search the
    Gospel namespace. If no variable is found, an exception is raised. *)
let unique_var env ocaml_vals defs q =
  match q with
  | Parse_uast.Qid pid when Env.mem pid.pid_str env ->
      Tlocal (Env.find pid.pid_str env)
  | _ ->
      (* If [q] is of the form [Qdot] it cannot be a local variable *)
      let q, params, pty = fun_qualid ocaml_vals defs q in
      Tvar (q, params, pty)

let preid pid old_env env =
  let id = Ident.from_preid pid in
  let old_env', env' = add_spec_var id !old_env !env in
  old_env := old_env';
  env := env';
  id

(** [binder (pid, pty)] maps the string [pid.pid_str] to a fresh tagged
    variable, adds all the type variables present in [pty] to the local scope
    and turns [pty] into a tagged annotation. *)
let binder defs old_env env (pid, pty) =
  let id = preid pid old_env env in
  let pty = Option.map (unique_pty ~ocaml:false ~bind:true defs !env) pty in
  (id, pty)

let rec pat_desc defs old_env env p =
  match p.Parse_uast.pat_desc with
  | Pwild -> Pwild
  | Pid pid -> Pid (preid pid old_env env)
  | Pcast (p, pty) ->
      let pty = unique_pty ~ocaml:false ~bind:true defs !env pty in
      Pcast (pat defs old_env env p, pty)
  | Ptuple l -> Ptuple (List.map (pat defs old_env env) l)

and pat defs old_env env p =
  { pat_desc = pat_desc defs old_env env p; pat_loc = p.pat_loc }

(** [unique_term top defs old_env env t] returns a term where every variable in
    [t] has been replaced with a uniquely tagged variable. When we find a free
    variable, we first search the local scope. The local scope is made up of two
    environments: [old_env] and [env], where [old_env] are the variables that
    are in scope only inside of [old] tags and [env] are those that are
    available outside of it. The [old_env] environment should include all
    variables within [env]. If [old_env] is [None], then we are processing a
    term that is not allowed to use the [old] keyword (i.e. anything that is not
    a post condition). These environments include variables bound within the
    term as well as specification arguments.

    When a variable is not found then we search in the [defs] top level. When a
    variable from the top level is used, we must add its typing information to
    the AST. This is necessary for the type inference phase so that the Inferno
    solver knows the type of all the variables outside the scope of the term. *)
let rec unique_post_term defs old_env env t =
  (* To be used when we perform a local open. *)
  let unique_term_open defs = unique_post_term defs old_env env in
  (* When processing a node that introduces a new name to the scope,
     we have to update the local environment. *)
  let unique_term_let old_env env = unique_post_term defs old_env env in
  (* The namespace and environment remains constant in all other
     recursive calls. *)
  let unique_term = unique_post_term defs old_env env in
  let term_desc =
    match t.Parse_uast.term_desc with
    | Parse_uast.Ttrue -> Ttrue
    | Tfalse -> Tfalse
    | TTrue -> TTrue
    | TFalse -> TFalse
    | Tconst c -> Tconst c
    | Tvar q -> unique_var env.term_var env.ocaml_vals defs q
    | Tlet (v, t1, t2) ->
        let env = ref env in
        let old_env = ref old_env in
        let ids = pat defs old_env env v in
        let t1 = unique_term t1 in
        (* Add the identifier to the local environment *)
        let t2 = unique_term_let !old_env !env t2 in
        Tlet (ids, t1, t2)
    | Tapply (t1, t2) -> Tapply (unique_term t1, unique_term t2)
    | Tinfix _ -> (unique_term (Uast_utils.chain t)).term_desc
    | Tquant (q, l, t) ->
        let old_env = ref old_env in
        let env = ref env in
        let l = List.map (binder defs old_env env) l in
        let t = unique_term_let !old_env !env t in
        Tquant (q, l, t)
    | Tif (g, then_b, else_b) ->
        let g = unique_term g in
        let then_b = unique_term then_b in
        let else_b = unique_term else_b in
        Tif (g, then_b, else_b)
    | Ttuple l -> Ttuple (List.map unique_term l)
    | Tlambda (args, t, pty) ->
        let old_env = ref old_env in
        let env = ref env in
        let pty =
          Option.map (unique_pty ~ocaml:false ~bind:true defs !env) pty
        in
        let args = List.map (pat defs old_env env) args in
        let t = unique_term_let !old_env !env t in
        Tlambda (args, t, pty)
    | Trecord l ->
        let qids, info =
          Namespace.fields_qualid ~loc:t.term_loc defs (List.map fst l)
        in
        let values =
          List.map2 (fun (id, ty) (_, t) -> (id, unique_term t, ty)) qids l
        in
        Trecord (values, info)
    | Tfield (t, q) ->
        let qid, fty, record_ty = Namespace.get_field_info defs q in
        Tfield (unique_term t, record_ty, qid, fty)
    | Tattr (att, t) -> Tattr (att, unique_term t)
    | Tcast (t, pty) ->
        let ty = unique_pty ~ocaml:false ~bind:true defs env pty in
        let t = unique_term t in
        Tcast (t, ty)
    | Tscope (q, t) ->
        let q, defs = Namespace.local_open defs q in
        let t = unique_term_open defs t in
        Tscope (q, t)
    | Told t ->
        let env =
          match old_env with
          | None -> W.error ~loc:t.term_loc Invalid_old
          | Some env -> env
        in
        let t = unique_term_let None env t in
        Told t
  in
  { term_desc; term_loc = t.term_loc }

(** [unique_term defs env t] is to be used for any term that is not a post
    condition.*)
let unique_term defs env t = unique_post_term defs None env t

let unique_post_term defs pre_env post_env t =
  unique_post_term defs (Some pre_env) post_env t

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
  let unique_pty = unique_pty ~ocaml:false ~bind:true defs in
  let fun_type = Option.map (unique_pty !env) f.fun_type in
  let fun_params =
    List.map
      (fun (pid, pty) ->
        let id = Ident.from_preid pid in
        (* Add the function parameters to the local environment. *)
        env := add_term_var id !env;
        (* Add the type variables in [pty] to the local environment. *)
        let pty = unique_pty !env pty in
        (id, pty))
      f.fun_params
  in
  let fun_spec = fun_spec defs !env f.fun_spec in
  let () =
    if fun_rec then
      (* If the function is recursive, add it as a local variable. *)
      env := add_term_var fun_name !env
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
let tdecl_env ~ocaml (l : Parse_uast.s_type_declaration list) =
  let open Parse_uast in
  let add_decl env t =
    (* Check if [t] is a type alias. *)
    if Option.is_none t.tmanifest then
      let id = Ident.from_preid t.tname in
      add_type_nm ~ocaml id env
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

    [type t3 = {x : t2} and t2 = t1 and t1 = t3]

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

let type_kind ~ocaml env tid tparams lenv = function
  | Parse_uast.PTtype_abstract -> (env, PTtype_abstract)
  | PTtype_record l ->
      let open Parse_uast in
      (* Function to create a unique record field *)
      let field l =
        let id = Ident.from_preid l.pld_name in
        let pty = unique_pty ~ocaml ~bind:false (scope env) lenv l.pld_type in
        {
          Id_uast.pld_name = id;
          pld_mutable = l.pld_mutable;
          pld_type = pty;
          pld_loc = l.pld_loc;
        }
      in
      let fields = List.map field l in
      let fields_id =
        List.map (fun l -> (l.Id_uast.pld_name, l.pld_type)) fields
      in
      (* If this is an OCaml record declaration, the record fields are not
         inserted into the namespace. *)
      let env = if ocaml then env else add_record env tid tparams fields_id in
      (env, PTtype_record fields)

let unique_tspec env model local_env self_ty inv_ty tspec =
  (* [invariants [pid, l] processes a list of invariants for the give type
     where [pid] is a variable of type [self_ty]. *)
  let invariants (pid, l) =
    let id = Ident.from_preid pid in
    let self_ty =
      match inv_ty with
      | Some t -> t
      | None -> Types.ocaml_no_model pid.pid_loc self_ty
    in
    let inv t =
      let local_env = add_term_var id local_env in
      let t = unique_term env local_env t in
      let tvars = get_tvars local_env in
      Solver.invariant tvars id self_ty t
    in
    (id, List.map inv l)
  in
  Tast.mk_type_spec tspec.Parse_uast.ty_mutable
    (Option.map invariants tspec.ty_invariant)
    model tspec.ty_text tspec.ty_loc

(** [create_model env lenv tname tparams tspec] Processes the model field(s) of
    the type specification [tspec]. If [tspec] has named model fields, we create
    a Gospel record with the same name as the OCaml type whose fields have the
    same names as the model and add it to [env]. Otherwise, we return [env]
    unchanged. *)
let create_model env lenv tname tparams tspec =
  let open Parse_uast in
  let unique_pty = unique_pty ~ocaml:false ~bind:false (scope env) lenv in
  match tspec with
  | None -> (env, Id_uast.No_model)
  | Some t -> (
      let model = t.ty_model in
      match model with
      | No_model -> (env, No_model)
      | Implicit pty ->
          let mpty = unique_pty pty in
          (env, Implicit mpty)
      | Fields l ->
          let model_tname = Ident.from_preid tname in
          let l =
            List.map (fun (id, pty) -> (Ident.from_preid id, unique_pty pty)) l
          in
          let env = Namespace.add_gospel_type env model_tname tparams None in
          let env = Namespace.add_record env model_tname tparams l in
          (env, Fields l))

(** [can_be_owned tkind talias spec] Checks if the OCaml type definition [tkind]
    coupled with the specifications [tspec] and type alias [talias] denote a
    type that can be "owned", in other words, a type whose inhabitants can
    appear in ownership clauses. This will return true if there was a [mutable]
    annotation in [tspec] or if the type definition [tkind] uses mutability in
    any way. *)
let can_be_owned tkind talias tspec =
  (* The mutable flag the user specified *)
  let spec_mut =
    Option.fold ~some:(fun s -> s.Parse_uast.ty_mutable) ~none:false tspec
  in
  (* If the type is not abstract, this will mark if the flag . *)
  let kind_mut =
    match tkind with
    | PTtype_abstract ->
        (* If the type is abstract, check if its alias is mutable (if it exists). *)
        Option.map Uast_utils.can_own talias
    | PTtype_record l ->
        (* Check if any of the fields are mutable*)
        let mut = List.exists (fun l -> l.pld_mutable = Mutable) l in
        (* If the fields themselves are not mutable, check if the
           types of the fields are mutable. *)
        let mut =
          mut || List.exists (fun l -> Uast_utils.can_own l.pld_type) l
        in
        Some mut
  in
  match (spec_mut, kind_mut) with
  | true, Some false ->
      (* If the specification states that the value is mutable but the
         OCaml type definition is not impure, we raise an
         exception. *)
      W.error ~loc:(Option.get tspec).ty_loc W.Bad_mutable_annotation
  | _, None ->
      (* If the type is abstract, we use the annotation the user
         provides.  If the user did not provide a [mutable]
         annotation, we assume it is not mutable. *)
      spec_mut
  | _, Some kind_mut ->
      (* If the type is not abstract, we use the type definition to
         check if it is mutable or not. *)
      kind_mut

(** [type_decl lenv env t] processes the type declaration [t] and adds it to
    [env]. Additionally, if the typed specification of [t] contains named
    models, we also add to [env] a record type with the same model fields as
    [t].

    This function also returns a closure which, when given an environment, will
    generate a typed declaration. The reason we do not generate it immediately
    is because the type specification of [t] may refer to model fields that have
    not been introduced into the scope yet. *)
let type_decl ~ocaml lenv env t =
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
  let lenv = { lenv with type_vars = Hashtbl.create 100 } in
  let () = List.iter (fun param -> add_type_var param lenv) tparams in
  (* We call [unique_pty] with the [bind] flag to false so that an error is
     raised if any type variables besides those defined in [tparams] are used. *)
  let tmanifest =
    Option.map (unique_pty ~ocaml ~bind:false defs lenv) t.tmanifest
  in

  let env, tkind = type_kind ~ocaml env tname tparams lenv t.tkind in
  let env, model = create_model env lenv t.tname tparams t.tspec in
  let mut = can_be_owned tkind tmanifest t.tspec in

  let tvars = List.map (fun x -> PTtyvar x) tparams in

  (* The [pty] object that represents this type name. This is necessary for the
     typed specification, where there will always be a variable of this type. *)
  let model_ty =
    match model with
    | Implicit pty -> Some pty
    | Fields _ ->
        (* If the model has multiple named fields, we fetch the record type
          created by [create_model]. *)
        let t =
          Namespace.resolve_application ~ocaml:false (scope env) (Qid t.tname)
            tvars
        in
        Some (PTtyapp (t, tvars))
    | No_model -> Option.bind tmanifest Uast_utils.ocaml_to_model
  in

  (* Closure that generates the typed declaration. Note that all this closure
     does is process the type specification. *)
  let def_gen =
    let info = Types.mk_info ~alias:tmanifest ~model:model_ty (Qid tname) in
    let self_ty = PTtyapp (info, tvars) in
    (* The [pty] object that represents values of this type within
       type invariants, if there are any. If this is an OCaml type, we
       use its model, if it exists. If it is a Gospel type, we use the
       type itself *)
    let inv_ty = if ocaml then model_ty else Some self_ty in
    fun env ->
      let tspec =
        Option.fold ~none:Tast.empty_tspec
          ~some:(unique_tspec (scope env) model lenv self_ty inv_ty)
          t.tspec
      in
      Tast.mk_tdecl tname tparams tkind tmanifest t.tattributes tspec t.tloc
  in
  (* Update the environment depending on whether or not this is an OCaml
     definition. *)
  let env =
    if ocaml then add_ocaml_type env tname tparams ~mut tmanifest model_ty
    else add_gospel_type env tname tparams tmanifest
  in
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
  let get_fields = function PTtype_record l -> l | _ -> [] in
  let all_fields = List.concat_map (fun decl -> get_fields decl.tkind) l in
  let field_error l =
    raise
      (W.error ~loc:l.pld_loc (W.Duplicated_record_label l.pld_name.pid_str))
  in
  Utils.duplicate
    (fun x y -> Preid.eq x.pld_name y.pld_name)
    field_error all_fields;
  Utils.duplicate eq t_error l

(** [tdecl_list env l] processes a list of mutually recursive type definitions.
    It does this by:

    - Checking if there are duplicate type names or record labels.
    - Creating a local environment which tracks the non-aliased type names
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
let tdecl_list ~ocaml env l =
  let open Parse_uast in
  (* Create a local environment with the non-aliased type names introduced by
     [l]. *)
  let () = tdecl_duplicates l in
  let lenv = tdecl_env ~ocaml l in

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
        let env, def = type_decl ~ocaml lenv env def in
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

let process_exception exn defs =
  let exn_id = Ident.from_preid exn.Parse_uast.exn_id in
  let lenv = empty_local_env () in
  let exn_args =
    List.map (unique_pty ~ocaml:true ~bind:true (scope defs) lenv) exn.exn_args
  in
  let env = add_exn defs exn_id exn_args in
  let exn =
    {
      exn_id;
      exn_args;
      exn_attributes = exn.exn_attributes;
      exn_loc = exn.exn_loc;
    }
  in
  (Tast.Sig_exception exn, env)

(* -------------------------------------------------------------------------- *)

(* Value descriptions. *)

(** [header_length l] takes the list of arguments (or return values) in a header
    and counts the amount of non-ghost variables. *)
let rec header_length = function
  | [] -> 0
  | x :: t -> (
      header_length t + match x with Parse_uast.Lghost _ -> 0 | _ -> 1)

(** [valid_val_n ~loc hd_vals args error] takes checks if the amount of
    arguments provided in [hd_vals] is valid with regards to the list of OCaml
    types [args].

    In case of a mismatch, we raise the Gospel exception returned by [error]
    when calling it with the expected and received values in that order. If
    there is no mismatch, this function has no effect. *)
let valid_val_n ~loc hd_vals args error =
  let expected = List.length args in
  let received = header_length hd_vals in
  if expected <> received then W.error ~loc (error expected received)

(** [valid_header header types rets] Receives the [header] for a value
    declaration that receives arguments of [types] and returns values of type
    [rets] (if the function does not return a tuple, this list will be of length
    one). Raises a Gospel exception if:
    - The list of arguments and return values contain any duplicates.
    - The number of arguments or return values is invalid for the declared type.
    - The value name in the header does not match the name in the Gospel
      interface file.

    If none of these conditions are met, this function has no observable effect.
*)
let valid_header loc header val_nm types rets xspecs =
  (* Check if the header name is the same as the OCaml value's name. *)
  let open Parse_uast in
  let header_nm = header.sp_hd_nm in
  if not (Preid.eq header_nm val_nm) then
    raise
      (W.error ~loc:header_nm.pid_loc (W.Invalid_header_name header_nm.pid_str));
  (* Function for raising an error for a duplicate variable. *)
  let dup_var_error l =
    let l = Option.get (Uast_utils.get_name l) in
    raise (W.error ~loc:l.pid_loc (W.Duplicated_header_value l.pid_str))
  in
  (* Check for duplicate arguments and return values. Return values
     can have the same name as long as they do not appear in the same
     branch (i.e. belong to different exceptional specifications).  *)
  let xspecs = List.map (fun x -> x.sp_xrets) xspecs in
  let check_dup rets =
    Utils.duplicate Uast_utils.v_eq dup_var_error (rets @ header.sp_hd_args)
  in
  let () = List.iter check_dup (header.sp_hd_ret :: xspecs) in
  (* Check if the number of arguments and return values is congruent
     with the value's type *)
  valid_val_n ~loc header.sp_hd_args types (fun n1 n2 ->
      W.Invalid_arg_number (n1, n2));
  (* If the function returns a tuple, the user is still allowed to
     give only one return value. *)
  if header_length header.sp_hd_ret <> 1 then
    valid_val_n ~loc header.sp_hd_ret rets (fun n1 n2 ->
        W.Invalid_ret_number (n1, n2))

(** [is_unit ty] Checks if the resolved type [ty] is [unit]. *)
let is_unit = function
  | PTtyapp ({ app_qid = Qid id; app_alias = None; _ }, []) ->
      Ident.equal Namespace.unit_id id
  | _ -> false

(** [pair_hd_vars ~loc types vars] Traverses the [vars] and [types], where
    [vars] are values (either arguments or return values) defined in a
    specification header and [types] are the OCaml types of the values, and
    creates an association list that maps each identifier to its type. Ghost
    arguments are ignored at this stage. *)
let rec pair_hd_vars types vars =
  match (types, vars) with
  | [], [] -> []
  | _, Parse_uast.Lghost _ :: t ->
      (* Ghost values are ignored at this stage. *)
      pair_hd_vars types t
  | ty :: types, id :: ids -> (
      match id with
      | Lwild ->
          (* Wildcard arguments are always valid *)
          pair_hd_vars types ids
      | Lunit loc ->
          (* A unit argument in the header is only valid if the
             argument's type is also unit *)
          if is_unit ty then pair_hd_vars types ids
          else Types.invalid_header_unit ~loc ty
      | Lvar s ->
          let id = Ident.from_preid s in
          (id, ty) :: pair_hd_vars types ids
      | Lghost _ ->
          (* Ghost values are handled in the previous branch*)
          assert false)
  | _ -> assert false (* Both lists must be of the same length. *)

module Tbl = Ident.IdTable

type owned_variables = {
  global : (qualid * Id_uast.pty) list;
  header : Ident.t list;
}

(** [resolve_vars defs vars dup_error l] traverses the list of variables [l] and
    returns the corresponding variable in [vars] or in the top level environment
    [defs].

    If any of the variables in [l] are not defined, this function raises a
    Gospel exception. Additionally, if there are duplicates in [l], the
    [dup_error] function is called to produce an exception of type [W.error]
    and, naturally, raise the produced exception. *)
let resolve_vars defs vars dup_error l =
  (* [resolve_var qid] finds the variable [qid] in [vars] list. *)
  let resolve_var qid =
    let find pid = fun (x, _) -> x.Ident.id_str = pid.Preid.pid_str in
    (* The resolved identifier with its type. *)
    let id, ty, global =
      match qid with
      | Parse_uast.Qid pid when Option.is_some (List.find_opt (find pid) vars)
        ->
          (* Variables defined in the header. *)
          let id, ty = List.find (find pid) vars in
          (Qid { id with id_loc = pid.pid_loc }, ty, false)
      | _ ->
          (* Top level variables. *)
          let qid, ty_ocaml = ocaml_val_qualid defs qid in
          (qid, ty_ocaml, true)
    in
    (* If variables of type [ty] cannot appear in ownership clauses
       (e.g. arrow types), then we raise a Gospel exception. *)
    if not (Uast_utils.can_own ty) then
      W.error ~loc:(Uast_utils.qualid_loc id)
        (W.Cant_be_owned (Uast_utils.flatten_ident id));
    if global then Either.left (id, ty) else Either.right (Uast_utils.leaf id)
  in
  let global, header = List.partition_map resolve_var l in
  (* Check for duplicates in the list of resolved variables. *)
  let error qid =
    let loc = (Uast_utils.leaf qid).id_loc in
    W.error ~loc (dup_error (Uast_utils.flatten_ident qid))
  in
  (* Check for duplicate ownership clauses *)
  let () =
    Utils.duplicate (fun x y -> Ident.equal x y) (fun x -> error (Qid x)) header
  in
  let () =
    Utils.duplicate
      (fun (x, _) (y, _) -> Uast_utils.eq_qualid x y)
      (fun (x, _) -> error x)
      global
  in
  { global; header }

(** [duplicated_ocaml_var l1 l2 error] Receives two lists of OCaml variables and
    raises a Gospel exception (generated by the [error] function, which receives
    a list of strings that represents the qualified identifier) if there is a
    common element between the two lists. *)
let common_value l1 l2 error =
  Seq.iter
    (fun x ->
      if Seq.exists (Uast_utils.eq_qualid x) l1 then
        W.error ~loc:(Uast_utils.leaf x).id_loc
          (error (Uast_utils.flatten_ident x)))
    l2

let duplicated_owned o1 o2 error =
  (* Helper functions to turn [owned_variables] into sequences that can
   be used by [duplicated_ocaml_var] *)
  let header_to_seq l = List.to_seq l |> Seq.map (fun x -> Qid x) in
  let global_to_seq l = List.to_seq l |> Seq.map fst in
  let own_to_seq o =
    Seq.append (header_to_seq o.header) (global_to_seq o.global)
  in
  common_value (own_to_seq o1) (own_to_seq o2) error

(** [process_sugar_ownership vspec vars] resolves the variables in the
    [modifies] and [preserves] clauses in [vspec] using the [vars] list and the
    [defs] top level environment. Additionally, creates a list of read only
    variables from the list of [preserves] clauses. This function also catches
    the following errors: - Two [modifies] clauses for the same variable. - Two
    [preserves] clauses for the same variable. - A [preserves] and a [modifies]
    clause for the same variable. *)
let process_sugar_ownership defs spec vars =
  (* Functions that return an exception in case of a duplicated
     [modifies] or [preserves] clauses. *)
  let dup_mod id = W.Duplicated_modifies id in
  let dup_pres id = W.Duplicated_preserves id in
  let pres_mod id = W.Modified_and_preserved id in
  (* Name resolution for names in [modifies] and [preserves] clauses.
     Also checks if there are any duplicates. *)
  let preserved_vars =
    resolve_vars defs vars dup_pres spec.Parse_uast.sp_preserves
  in
  let modified_vars = resolve_vars defs vars dup_mod spec.sp_modifies in
  (* Checks if there is a value that is both in a [preserves] and
     [modifies] clause. *)
  let () = duplicated_owned preserved_vars modified_vars pres_mod in
  (modified_vars, preserved_vars)

(** [process_ownership ~consume defs mod_and_pres own vars] processes the
    ownership clauses [own] (which is either a list of [produces] or a list of
    [consumes] clauses). Performs name resolution using the [vars] list and the
    [defs] top level environment and pair each name with its its respective
    OCaml type. Also checks if the variable appears in the [mod_and_pres] list,
    which should be the list of variables in [modifies] and [preserves] clauses.
    The [consumes] clause only impacts error messages. *)
let process_ownership ~consumes defs modifies preserves own vars =
  (* Functions that return (not raise) an exception for errors in
     [consumes] or [produces] clauses.  *)
  let dup_error qid =
    if consumes then W.Duplicated_consumes qid else W.Duplicated_produces qid
  in
  let mod_error qid =
    if consumes then W.Desugared_consumes qid else W.Desugared_produces qid
  in
  let owned_variables = resolve_vars defs vars dup_error own in
  (* Checks if there is a variable also appears in a [modifies] or
     [preserves] clause. *)
  let () = duplicated_owned owned_variables modifies mod_error in
  let () = duplicated_owned owned_variables preserves mod_error in
  {
    header = owned_variables.header @ modifies.header @ preserves.header;
    global = owned_variables.global @ modifies.global @ preserves.global;
  }

(** [add_values_env header lenv defs vals consumes produces read_only] creates
    two environments to process OCaml value specifications: one for pre
    conditions (and old annotations) and another for post conditions. Both
    environments begin with the same value as [lenv], which at this point should
    only contain type variables. The type variables table is shared between both
    environments.

    For the environment for pre conditions, we add all the OCaml variables in
    [consumes] list. For the environment for post conditions, we add all the
    OCaml variables in [produces] as well as [consumes]. Ghost variables are
    added to both environments

    This function returns a list of [sp_var] which contains the information
    regarding each variable in the header. *)
let add_values_env hd_args hd_rets lenv defs args rets consumes produces
    read_only =
  (* [process_var pre_env post_env v] creates an [sp_var] for the
     header variable [v] and adds it to the [pre_env] and
     [post_env].  *)
  let process_var pre_env post_env vals = function
    | Parse_uast.Lwild -> (Wildcard, pre_env, post_env)
    | Lunit _ -> (Unit, pre_env, post_env)
    | Lghost (pid, pty) ->
        let id = Ident.from_preid pid in
        let pty = unique_pty ~ocaml:false ~bind:true defs lenv pty in
        let pre_env, post_env = add_spec_var id (Some pre_env) post_env in
        (Ghost (id, pty), Option.get pre_env, post_env)
    | Lvar pid ->
        (* Finds the OCaml type and the unique identifier for [pid].
           This lookup always succeeds. *)
        let var_mem = fun id -> pid.pid_str = id.Ident.id_str in
        let var_name, ty_ocaml = List.find (fun (id, _) -> var_mem id) vals in

        (* Creates the Gospel representation for this OCaml value. *)
        let ty_gospel = Uast_utils.ocaml_to_model ty_ocaml in
        let has_gospel = Option.is_some ty_gospel in
        (* Checks if the function receives or returns ownership of this value. *)
        let cons = List.exists var_mem consumes in
        let prod = List.exists var_mem produces in
        (* Check if this value is modified. *)
        let ro = List.exists var_mem read_only in
        (* Adds the variable to the pre and post environment if it is
           consumed or produced, respectively.  Additionally, if the
           variable does not have a valid gospel representation, it is
           never added to either environment. *)
        let add_var ~add env =
          if add && has_gospel then add_term_var var_name env else env
        in
        ( OCaml { var_name = Qid var_name; ty_ocaml; ty_gospel; prod; cons; ro },
          add_var ~add:cons pre_env,
          add_var ~add:prod post_env )
  in
  (* [process_vars pre_env post_env l] processes a list of header variables. *)
  let rec process_header pre_env post_env vals = function
    | [] -> ([], pre_env, post_env)
    | x :: t ->
        let var, pre_env, post_env = process_var pre_env post_env vals x in
        let vars, pre_env, post_env = process_header pre_env post_env vals t in
        (var :: vars, pre_env, post_env)
  in
  let args, pre_env, post_env = process_header lenv lenv args hd_args in
  let rets, pre_env, post_env = process_header pre_env post_env rets hd_rets in
  (args, rets, pre_env, post_env)

(** If an OCaml argument or return value does not appear in an ownership clause,
    we assume, in the case of an argument, a [preserves] clause and, in the case
    of a return value, a [produces] clause. *)
let insert_ownership consumes produces ro args rets =
  (* [is_own q1] checks if [q1] is in any ownership clause.  If so,
     returns [None]. *)
  let is_own (id, _) =
    (* Check if [q1] is in either the [produces] or [consumes]. *)
    let eq = Ident.equal id in
    if List.exists eq produces || List.exists eq consumes then None else Some id
  in
  (* We filter the [args] and [rets] list so that we only get the
     variables that are not in any ownership clause. *)
  let args = List.filter_map is_own args in
  let rets = List.filter_map is_own rets in
  (* Since the values in [args] are not in any ownership clause, they
     are preserved, meaning they are consumed, produced and
     unmodified.  The values in [rets] are only produced. *)
  (args @ consumes, args @ rets @ produces, args @ ro)

(** [valid_pure loc args rets tops ~diverge ~pure] Checks if the function is
    pure or not. A function is pure if it does not modify any top level variable
    or any of its arguments. Naturally, a pure function cannot diverge.

    This function raises a Gospel exception if the user added a [pure] to the
    specification but the function does not meet the previous criteria. *)
let valid_pure loc args tops xspec ~diverge ~pure =
  let is_mut = function OCaml v -> not v.ro | _ -> false in
  (* If the function is marked as pure but at least one of the
     variables used by this function is modified, we raise an
     exception *)
  let mut = List.exists is_mut args || List.exists (fun x -> not x.ro) tops in
  let raises = xspec <> [] in
  if pure && mut then W.error ~loc W.Pure_modifies;
  if pure && diverge then W.error ~loc W.Pure_diverges;
  if pure && raises then W.error ~loc W.Pure_raises;
  not (mut || diverge || raises)

(** [returns_unit rets] checks if the list of return values is the singleton
    list composed of the [unit] type. *)
let returns_unit = function [ r ] when is_unit r -> true | _ -> false

let type_val_spec spec defs args rets tops xspec pre_env post_env =
  let pre =
    List.map (unique_term defs pre_env) spec.Parse_uast.sp_pre_spec.sp_pre
  in
  let checks =
    List.map (unique_term defs pre_env) spec.Parse_uast.sp_pre_spec.sp_checks
  in
  let post =
    List.map (unique_post_term defs pre_env post_env) spec.sp_post_spec.sp_post
  in
  let spec =
    {
      sp_args = args;
      sp_rets = rets;
      sp_tops = tops;
      sp_pre = pre;
      sp_checks = checks;
      sp_post = post;
      sp_xpost = xspec;
      sp_diverge = spec.sp_pre_spec.sp_diverge;
      sp_pure = spec.sp_pre_spec.sp_pure;
      sp_text = spec.sp_text;
      sp_loc = spec.sp_loc;
    }
  in
  Solver.spec (get_tvars pre_env) spec

(** [global_values pre_tbl post_tbl consumes produces modifies preserves]
    returns the list of top level variables that are used in ownership clauses.
    Also populates the [pre_tbl] and the [post_tbl] with the top level variables
    that can be used in pre and post conditions, respectively. *)
let global_values_list pre_tbl post_tbl consumes produces modifies preserves =
  let mem x = fun (y, _) -> Uast_utils.eq_qualid x y in
  (* The produces list without any value from the [consumes] list.  At
     this point, there cannot be any duplicates in any other list. *)
  let prod_no_cons =
    List.filter (fun (x, _) -> List.exists (mem x) consumes) produces
  in
  List.map
    (fun (var_name, ty_ocaml) ->
      (* Get the Gospel model of the type. *)
      let ty_gospel = Uast_utils.ocaml_to_model ty_ocaml in
      (* Check if the values are consumed and produced. *)
      let var_tag = (Uast_utils.leaf var_name).id_tag in
      let prod = List.exists (mem var_name) produces in
      let cons = List.exists (mem var_name) consumes in
      (* Populates the [pre_tbl] and [post_tbl]. *)
      if cons then Option.iter (Tbl.add pre_tbl var_tag) ty_gospel;
      if prod then Option.iter (Tbl.add post_tbl var_tag) ty_gospel;
      {
        var_name;
        ty_ocaml;
        ty_gospel;
        prod;
        cons;
        ro = List.exists (mem var_name) preserves;
      })
    (consumes @ prod_no_cons @ modifies @ preserves)

(** [process_produces defs lenv produces hd_args hd_rets consumes modifies
     preserves args rets] processes the [produces] clause of a specification. It
    first performs name resolution on the values in the [produces] list.

    Any header variable that is not present in an ownership clause is assumed to
    be consumed, produced and read only (effectively a [preserves]). Once we
    have the full list of consumed and produced variables, we create the typed
    representation for each argument, return value (represented by [sp_var]) and
    top level variables (represented by [sp_ocaml_var]). We also create two
    local environments: one with the variables that are consumed (for
    preconditions) and another with the variables that are produced and consumed
    (for postconditions).

    This function is used for processing both exceptional and non-exceptional
    specifications, since they are handled the same way. The only difference is
    that [hd_rets], in case of an exceptional specification, should be the
    exceptional return values and [produces] should be the [xproduces] list. *)
let process_produces defs lenv produces hd_args hd_rets consumes modifies
    preserves args rets =
  let produces =
    process_ownership ~consumes:false defs modifies preserves produces
      (args @ rets)
  in
  (* Augment the [consumes], [produces] and [read_only] lists with all
     the variables that are named in the header but do not appear in
     an ownership clause.  At this point, each variable named in the
     header appears in the [produces] or [consumes] list (or both).

     Note: since immutable variables cannot appear in an ownership
     clause, these will always be automatically added to the list of
     consumed, produced and read only variables*)
  let consumes_header, produces_header, read_only =
    insert_ownership consumes.header produces.header preserves.header args rets
  in
  (* Creates environments with the variables defined in the header. *)
  let sp_args, sp_rets, pre_env, post_env =
    add_values_env hd_args hd_rets lenv defs args rets consumes_header
      produces_header read_only
  in
  let tops =
    global_values_list pre_env.ocaml_vals post_env.ocaml_vals consumes.global
      produces.global modifies.global preserves.global
  in
  (sp_args, sp_rets, tops, pre_env, post_env)

(** [type_xspec defs lenv modifies preserves consumes hd_args args xspec] type
    checks the exceptional specification [xspec]. The process is effectively the
    same as that for normal post conditions. *)
let type_xspec defs lenv modifies preserves consumes hd_args args xspec =
  let sp_exn, ret_type = get_exn_info defs xspec.Parse_uast.sp_exn in
  (* In exceptional specifications, the user is always allowed to
     provide a wildcard value or no return values. *)
  if not (xspec.sp_xrets = [] || xspec.sp_xrets = [ Lwild ]) then
    valid_val_n ~loc:xspec.sp_xloc xspec.sp_xrets ret_type (fun n1 n2 ->
        W.Invalid_exn_ret_number (n1, n2));
  let xrets =
    if xspec.sp_xrets = [] || xspec.sp_xrets = [ Lwild ] then []
    else pair_hd_vars ret_type xspec.sp_xrets
  in
  let sp_xargs, sp_xrets, sp_xtops, pre_env, post_env =
    process_produces defs lenv xspec.sp_xproduces hd_args xspec.sp_xrets
      consumes modifies preserves args xrets
  in
  let sp_xpost =
    List.map (unique_post_term defs pre_env post_env) xspec.sp_xpost
  in
  { sp_exn; sp_xargs; sp_xrets; sp_xtops; sp_xpost; sp_xloc = xspec.sp_xloc }

(** [ocaml_value env v] Translates an OCaml value description to a Gospel value
    description. This is done by:

    - Creating a header for the specification if it does not have one.
    - Checking for errors in the header (for more details, check the
      documentation for [valid_header]).
    - Turning the OCaml type of the value into a list of arguments and return
      values and associating each non-ghost name in the header with its OCaml
      type.
    - Checks if the ownership clauses are valid (no duplicates and all the names
      are OCaml variables that are in scope).
    - Augmenting the list of consumed and produced values with the variables
      named in the header that do not appear in an ownership clause.
    - Turning the OCaml types of the function's arguments and return values into
      Gospel types.
    - Creating the local environments for processing pre and post conditions
      using the information from the ownership clauses (e.g. a value that is not
      consumed cannot be used in a precondition).
    - Checking if the [pure] and [diverge] annotations are valid. Also ensure
      that functions that return [unit] perform some effect.
    - Typechecking the pre and post conditions and creating the typed value
      description.
    - Adding the OCaml value into the namespace [env]

    Note: Exceptional specifications are type checked the same way as normal
    post conditions, the only thing that changes is the list of produced
    variables as well as the list of return values. *)
let value_spec ~loc defs lenv name ocaml_ty spec =
  (* From the function's type, get the types of the values' arguments *)
  let arg_types, ret_types = Uast_utils.args_to_list ocaml_ty in
  (* Create a header if the user did not provide one. *)
  let header =
    Option.fold
      ~none:(Uast_utils.create_header name (List.length arg_types))
      ~some:(fun header ->
        (* If the user has provided a header, then we check if there
             are any errors. *)
        valid_header loc header name arg_types ret_types
          spec.Parse_uast.sp_xpost_spec;
        header)
      spec.sp_header
  in

  (* If the function returns a tuple but the user only supplied one
       return value, then the list of return types will be a singleton
       list with a tuple. *)
  let ret_types =
    if List.length ret_types > 1 && header_length header.sp_hd_ret = 1 then
      [ PTtuple ret_types ]
    else ret_types
  in

  (* Creates association lists with the functions' arguments and
       return values mapped to their respective types.  *)
  let args = pair_hd_vars arg_types header.sp_hd_args in
  let rets = pair_hd_vars ret_types header.sp_hd_ret in
  (* Resolves the variables in [modifies] and [preserves] clauses.
       [mod_and_pres] contains all the variables in both [modifies] and
       [preserves] clauses, whereas [read_only] only contains those in
       [preserves] clauses. *)
  let modifies, preserves =
    process_sugar_ownership defs spec.sp_pre_spec args
  in
  (* Resolves the variables in [consumes] and [produces] clauses.
     Note how return values are not allowed to be consumed. *)
  let consumes =
    process_ownership ~consumes:true defs modifies preserves
      spec.sp_pre_spec.sp_consumes args
  in
  let sp_args, sp_rets, tops, pre_env, post_env =
    process_produces defs lenv spec.sp_post_spec.sp_produces header.sp_hd_args
      header.sp_hd_ret consumes modifies preserves args rets
  in
  (* Type check the exceptional specifications. *)
  let sp_xspec =
    List.map
      (type_xspec defs lenv modifies preserves consumes header.sp_hd_args args)
      spec.sp_xpost_spec
  in
  let () =
    Utils.duplicate
      (fun x y -> Uast_utils.eq_qualid x.sp_exn y.sp_exn)
      (fun x ->
        W.error ~loc:x.sp_xloc
          (Duplicate_exceptional_spec (Uast_utils.flatten_ident x.sp_exn)))
      sp_xspec
  in
  (* Raises a Gospel exception if the function is marked as pure when
       it is not allowed. *)
  let is_pure =
    valid_pure loc sp_args tops ~pure:spec.sp_pre_spec.sp_pure
      ~diverge:spec.sp_pre_spec.sp_diverge spec.sp_xpost_spec
  in
  (* Raises a Gospel exception if the function returns unit but none
       of its values are modified. *)
  let () =
    if is_pure && returns_unit ret_types then
      W.error ~loc:spec.sp_loc W.Cant_return_unit
  in
  (* Type checks the pre and post conditions. *)
  type_val_spec spec defs sp_args sp_rets tops sp_xspec pre_env post_env

let ocaml_val env v =
  let lenv = empty_local_env () in
  let unique_ocaml_pty = unique_pty ~ocaml:true ~bind:true (scope env) lenv in
  let vtype = unique_ocaml_pty v.Parse_uast.vtype in
  let vspec =
    Option.map (value_spec ~loc:v.vloc (scope env) lenv v.vname vtype) v.vspec
  in
  let vtvars = get_tvars lenv @ Option.fold ~none:[] ~some:snd vspec in
  let v =
    {
      Tast.vname = Ident.from_preid v.vname;
      vtype;
      vattributes = v.vattributes;
      vtvars;
      vspec = Option.map fst vspec;
      vloc = v.vloc;
    }
  in
  (Tast.Sig_value v, add_ocaml_val env v.vname vtvars vtype)

(* -------------------------------------------------------------------------- *)

let rec process_module env m =
  (* TODO figure out when this is None *)
  let id = Ident.from_preid m.Parse_uast.mdname in
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
  in
  (* Add the module to the environment. *)
  let env = add_mod env id mod_defs in
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
    | Sig_val v -> ocaml_val env v
    | Sig_type t ->
        let env, t = tdecl_list ~ocaml:true env t in
        (Tast.Sig_type t, env)
    | Sig_module m -> process_module env m
    | Sig_attribute att -> (Sig_attribute att, env)
    | Sig_exception exn -> process_exception exn env
    | _ -> assert false
  in
  ({ Tast.sdesc; sloc = s.sloc }, env)

and gospel_sig env = function
  | Parse_uast.Sig_function f ->
      let f, vars = function_ f (scope env) in
      let f, fun_ty = Solver.function_ vars f in
      let env = add_fun env f.fun_name f.fun_tvars fun_ty in
      (Tast.Sig_function f, env)
  | Sig_axiom ax ->
      (* Since axioms cannot be referenced, the environment is not
            modified.*)
      let ax, vars = axiom (scope env) ax in
      let ax = Solver.axiom vars ax in
      (Sig_axiom ax, env)
  | Sig_ghost_type t ->
      let env, t = tdecl_list ~ocaml:false env t in
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
