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

module String_list = struct
  type t = string list

  let compare l1 l2 = compare (List.sort compare l1) (List.sort compare l2)
end

module Record_env = Map.Make (String_list)
(** This module is used to map lists of record field names to their respective
    records. This list is used so that if there are multiple record labels with
    the same name in scope, we can pinpoint what record the user is trying to
    create. As an example:

    [module M1 : sig type t1 = {x : int; y : int} end]

    [module M2 : sig type t2 = {x : int; z : int} end]

    [module M3 : sig type t3 = {y : int; z : int} end]

    [open M1 open M2 open M3] [{x=0;y=0}]

    Although all three labels [x], [y] and [z] are defined multiple times, we
    can still figure out that the user wants to create a value of type [M1.t1]
    since the combination of labels [x, y] only occurs on this type. *)

module W = Warnings
open Id_uast

type fun_info = {
  fid : Ident.t;
  (* The unique identifier for this function. *)
  fparams : Ident.t list;
  (* All the type variables used in [fty]. *)
  fty : Id_uast.pty; (* The function's type. *)
}

type ty_info = {
  tid : Ident.t; (* The unique identifier for the type *)
  tparams : Ident.t list;
  (* The type variables this type takes as argument. We need to know the names
     of each parameter so that when we expand this type's alias, we know how to
     replace its type variables. e.g:

     [type ('a, 'b) t = 'a -> 'b]
     [function x : (int, string) t].

     When typechecking we replace [t] with its alias where every ['a]
     is replaced with [int] and ['b] is replaced with [string],
     resulting in the expanded type [int -> string]. *)
  tmut : bool;
  (* Mutability flag.  Always [false] for Gospel types. *)
  talias : Id_uast.pty option;
      (* In case of a type declaration of the form [type t = alias] where alias is
     some type expression, this value is [Some alias]. All applications of type
     [tid] are replaced with [talias] during typechecking.

     The [talias] field always points to the "top most" alias. This means if we
     have the following program [type t1 type t2 = t1 type t3 = t2] the [talias]
     field for [t3] will be [t1].*)
  tmodel : Id_uast.pty option;
      (* The logical representation for the type.  [None] if the type
      has no model or is a Gospel type. *)
}

type record_info = {
  rid : Ident.t; (* The name of the record type. *)
  rparams : Ident.t list;
  (* The type parameters for the record type. Should be equal to the [tparams]
       list in the entry for [rid] in the corresponding type environment. *)
  rfields : (Ident.t * Id_uast.pty) list; (* The list of all record fields. *)
}

type field_info = { rfid : Ident.t; rfty : Id_uast.pty; rfrecord : record_info }

type exn_info = {
  eid : Ident.t;
  eargs : Id_uast.pty list;
      (* The OCaml types of the arguments this exception receives. *)
}

type mod_info = { mid : Ident.t; mdefs : mod_defs }

and mod_defs = {
  (* Environments for Gospel definitions *)
  fun_env : fun_info Env.t; (* Function definitions *)
  type_env : ty_info Env.t; (* Type definitions *)
  field_env : field_info Env.t;
  (* Maps the name of each identifier for a record field to the corresponding
     [field_info] object.

     Invariant: for all values [rf] in the co-domain of [field_env], there
     exists one and only one value [r] in the co-domain of [record_env] where [r
     = rf.rfrecord].

     Invariant: the cardinality of [field_env] is greater or equal than the
     cardinality of [record_env]. *)
  record_env : record_info Record_env.t;
  (* Similar to [field_env], except now the domain is the list of record
     labels. This environment is used strictly for record creation.

     Invariant: for all values [r] in the co-domain of [record_env], there
     exists one and only one value [t] in the co-domain of [type_env] where
     [r.rid = t.tid]. Additionally, [r.rparams = t.tparams].

     Invariant: the cardinality of [record_env] is smaller or equal than
     the cardinality of [type_env]. *)
  (* Environments for OCaml definitions *)
  ocaml_type_env : ty_info Env.t;
  (* OCaml type definitions. *)
  ocaml_val_env : fun_info Env.t;
  exn_env : exn_info Env.t; (* Exceptions  *)
  mod_env : mod_info Env.t; (* Nested modules *)
}
(** Set of top level module definitions *)

let empty_defs =
  {
    fun_env = Env.empty;
    type_env = Env.empty;
    field_env = Env.empty;
    record_env = Record_env.empty;
    ocaml_type_env = Env.empty;
    ocaml_val_env = Env.empty;
    exn_env = Env.empty;
    mod_env = Env.empty;
  }

(* -------------------------------------------------------------------------- *)

(* The following section defines a functor [Lookup] for finding names in a
    [mod_defs] object. Regardless of whether an identifier [M1.M2...id] denotes
    a type, function, exception, etc... the logic is always the same: find the
    namespace denoted by [M1.M2...] and in that namespace find the identifier
    [id]. There are a few things that vary: notably the [mod_defs] field we use
    to lookup the name. *)

module type LDeps = sig
  type info
  (** The data associated with the identifier *)

  val id_lookup : info -> Ident.t
  (** [id_lookup info] returns the unique identifier from [info]. *)

  val env : mod_defs -> info Env.t
  (** [env ~ocaml defs] returns the environment in which the lookup will be
      performed. The [ocaml] parameter can be used to differentiate between the
      Gospel and OCaml namespace if such a distinction is necessary. *)

  val err : string list -> W.kind
  (** [err s] returns (not raises!) the Gospel error for the case in which the
      lookup fails. *)
end

(** The signature for refinements of [Lookup]. *)
module type L = sig
  type info

  val unique_toplevel_qualid :
    mod_defs -> Parse_uast.qualid -> Id_uast.qualid * info
  (** [unique_toplevel_qualid ~ocaml defs q] returns the fully resolved
      identifier for [q] in the namespace [mod_defs] as well as the data
      associated with it.

      @raise Warnings.Error
        if there is no identifier named [q] in namespace [mod_defs]. *)
end

module rec Lookup : functor (M : LDeps) -> L with type info = M.info =
functor
  (M : LDeps)
  ->
  struct
    type info = M.info

    let mk_qid pre id = match pre with None -> Qid id | Some q -> Qdot (q, id)

    let id_lookup info pid_loc =
      let id = M.id_lookup info in
      { id with id_loc = pid_loc }

    let unique_toplevel_qualid defs q =
      try
        let pre, pid, defs =
          match q with
          | Parse_uast.Qid pid -> (None, pid, defs)
          | Qdot (q, pid) ->
              let q, defs = Lookup_module.unique_toplevel_qualid defs q in
              (Some q, pid, defs.mdefs)
        in
        let info = Env.find pid.pid_str (M.env defs) in
        let id = id_lookup info pid.pid_loc in
        (mk_qid pre id, info)
      with Not_found ->
        let id = Uast_utils.flatten q in
        let loc = match q with Qid id | Qdot (_, id) -> id.pid_loc in
        W.error ~loc (M.err id)
  end

and Lookup_module : (L with type info = mod_info) = Lookup (struct
  type info = mod_info

  let id_lookup info = info.mid
  let env defs = defs.mod_env
  let err l = W.Unbound_module l
end)

(* -------------------------------------------------------------------------- *)

let access_mod defs q =
  let q, info = Lookup_module.unique_toplevel_qualid defs q in
  (q, info.mdefs)

module Lookup_gospel_type = Lookup (struct
  type info = ty_info

  let id_lookup info = info.tid
  let env defs = defs.type_env
  let err id = W.Unbound_type id
end)

let gospel_type_info = Lookup_gospel_type.unique_toplevel_qualid

module Lookup_ocaml_type = Lookup (struct
  type info = ty_info

  let id_lookup info = info.tid
  let env defs = defs.ocaml_type_env
  let err id = W.Unbound_type id
end)

let ocaml_type_info = Lookup_ocaml_type.unique_toplevel_qualid

module Lookup_exn = Lookup (struct
  type info = exn_info

  let id_lookup info = info.eid
  let env defs = defs.exn_env
  let err id = W.Unbound_exception id
end)

let get_exn_info env id =
  let id, info = Lookup_exn.unique_toplevel_qualid env id in
  (id, info.eargs)

module Tbl = Ident.IdTable

(** [map_tvars var_map alias] returns a new type with the same structure as
    [alias] where every type variable has been replaced with their binding in
    [var_map]. *)
let rec map_tvars var_tbl alias =
  let map_tvars = map_tvars var_tbl in
  match alias with
  | PTtyvar id ->
      (* The type variable [id] is replaced with its binding in the
         table [var_tbl]. *)
      Tbl.find var_tbl id.id_tag
  | PTarrow (t1, t2) -> PTarrow (map_tvars t1, map_tvars t2)
  | PTtyapp (q, l) -> PTtyapp (q, List.map map_tvars l)
  | PTtuple l -> PTtuple (List.map map_tvars l)

let resolve_application ~ocaml env q l =
  let q, info =
    if ocaml then ocaml_type_info env q else gospel_type_info env q
  in
  let params = info.tparams in
  let len1, len2 = (List.length params, List.length l) in
  if len1 <> len2 then (* type arity check *)
    W.error ~loc:Location.none (W.Bad_arity (info.tid.Ident.id_str, len1, len2));
  (* Auxiliary tables to map type variables to concrete types. *)
  let alias_tbl = Ident.IdTable.create 100 in
  let model_tbl = Ident.IdTable.create 100 in
  (* [populate ~f tbl tys] associates the type parameters [params]
     with its respective element in [tys].  The [f] function is a
     map/filter function that is called on each element of [tys].  If
     [f] returns [None], the respective type variable is not bound in
     the table. *)
  let populate ~f tbl tys =
    List.iter2
      (fun avar t -> Option.iter (Tbl.add tbl avar.Ident.id_tag) (f t))
      params tys
  in
  (* Map each type variable to the respective type in [l]. *)
  let () = populate ~f:(fun x -> Some x) alias_tbl l in
  (* Map each type variable to the logical representation of its
     respective type in [l].  If there is no logical representation,
     this variable will be unbound in [model_tbl]. *)
  let () = populate ~f:(fun x -> Uast_utils.ocaml_to_model x) model_tbl l in
  (* If the type [q] is an abbreviation for another type, we can
     always expand the abbreviation by replacing respective type
     variables with the applied type expressions [l]. *)
  let alias = Option.map (map_tvars alias_tbl) info.talias in
  let model =
    (* If the type [q] has a model, we try to build it using the
       logical representations of the type expressions [l]. This will
       result in a [Not_found] if any of the types in [l] that the
       model depends on do not have a logical representation. *)
    try Option.map (map_tvars model_tbl) info.tmodel with Not_found -> None
  in
  Types.mk_info q ~alias ~model ~mut:info.tmut

module Lookup_fun = Lookup (struct
  type info = fun_info

  let id_lookup info = info.fid
  let env defs = defs.fun_env
  let err id = W.Unbound_variable id
end)

let fun_info = Lookup_fun.unique_toplevel_qualid

module Lookup_val = Lookup (struct
  type info = fun_info

  let id_lookup info = info.fid
  let env defs = defs.ocaml_val_env
  let err id = W.Unbound_variable id
end)

let val_info = Lookup_val.unique_toplevel_qualid

let ocaml_val_check ocaml_vals env q =
  (* If there are no valid ocaml values, then no lookup is performed. *)
  if Tbl.length ocaml_vals = 0 then None
  else
    try
      (* Raises [W.Error] if [q] is not in the OCaml namespace. *)
      let q, _ = val_info env q in
      let pty = Tbl.find ocaml_vals (Uast_utils.leaf q).id_tag in
      Some (q, pty)
    with W.Error _ -> None

let fun_qualid ocaml_vals env q =
  match ocaml_val_check ocaml_vals env q with
  | None ->
      let q, info = fun_info env q in
      (q, info.fparams, info.fty)
  | Some (q, ty_gospel) -> (q, [], ty_gospel)

let ocaml_val_qualid env q =
  let q, info = val_info env q in
  assert (info.fparams = []);
  (q, info.fty)

(* [leaf q] returns an identifier string without its prefix. *)
let leaf = function
  | Parse_uast.Qdot (_, id) -> id.pid_str
  | Qid id -> id.pid_str

let fields_qualid ~loc defs fields =
  (* If we have a record creation where one of the fields is qualified, for
     example, [{M.x = 0; y = 0}], then we look for the definition of a record
     with labels [x] and [y] in module [M]. To do this, we first check if any
     field is of the form [M1.M2...]. We then use the environment associated
     with module [M1.M2...] to perform the lookup for the record. *)

  (* [qdot_mod q] returns the environment associated with the prefix of [q]. If
     the identifier has no prefix (i.e. is not of the form [M1.M2...]), this
     function returns [None]. *)
  let qdot_mod = function
    | Parse_uast.Qdot (q, _) -> Some (snd (access_mod defs q))
    | _ -> None
  in

  (* If there is an identifier in [fields] of the form [M1.M2...], returns the
     environment its prefix refers to. Otherwise, use the [defs] environment. *)
  let rdefs = Option.value ~default:defs (List.find_map qdot_mod fields) in

  (* Turn all field identifiers into unqualified strings. *)
  let labels = List.map leaf fields in

  (* Find the record object with the labels the user supplied in [env]. If this
     look up fails, it is because there is no record type in environment [rdefs]
     where the labels are equal to those in [labels]. TODO: in case of a
     [Not_found], we should give a more precise error message (e.g. are there
     fields missing, are the fields not defined etc). *)
  let find_labels env =
    try Record_env.find labels env
    with Not_found -> W.error ~loc W.Invalid_record_labels
  in

  let record = find_labels rdefs.record_env in
  (* [find_label id] finds corresponding identifier to [id] in [id_labels]. This
     look up always succeeds as long as [pid.pid_str] is one of the labels in the
     [labels] list. *)
  let find_label pid =
    List.find
      (fun (id, _) -> id.Ident.id_str = pid.Preid.pid_str)
      record.rfields
  in

  (* [resolve_field q] maps the identifier [q] with the type of its
     label. Additionally, if [q] is of type [M1.M2...], we check if the label is
     defined in module [M1.M2..]. *)
  let resolve_field q =
    match q with
    | Parse_uast.Qid pid ->
        (* If a field is not qualified, we look for the corresponding identifier in
          [id_labels]. This lookup always succeeds. *)
        let id, ty = find_label pid in
        (Qid id, ty)
    | Qdot (pre, pid) ->
        (* If a field is qualified, we check if we can reach [record] by
          following the modules in [q] in environment [defs]. If not, either
          the identifier [pre] does not denote a valid module, the record label
          [pid] does not exist in module [pre], or the record label [pid]
          belongs to another type. *)

        (* This lookup checks if the module [pre] exists. *)
        let pre_id, rdefs = access_mod defs pre in

        (* This lookup checks if there is a record with the same fields as
           [record] in module [pre].*)
        let r = find_labels rdefs.record_env in

        (* This lookup checks if the record that [q] belongs to is the same as
           [record]. *)
        let () =
          let open Uast_utils in
          if not (Ident.equal r.rid record.rid) then
            W.error ~loc
              (W.Incompatible_field
                 (flatten q, flatten pre @ [ r.rid.id_str ], record.rid.id_str))
        in

        (* If we reach this point, [pre] is a valid path to reach the field
          [pid]. We then look for the corresponding identifier in the [labels]
          list *)
        let id, ty = find_label pid in
        (Qdot (pre_id, id), ty)
  in

  let l = List.map resolve_field fields in
  let ty = { params = record.rparams; name = record.rid } in
  (l, ty)

module Lookup_field = Lookup (struct
  type info = field_info

  let id_lookup info = info.rfid
  let env defs = defs.field_env
  let err id = W.Unbound_record_label id
end)

let get_field_info defs q =
  let q, info = Lookup_field.unique_toplevel_qualid defs q in
  (q, info.rfty, { params = info.rfrecord.rparams; name = info.rfrecord.rid })

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

let add_fun fid tvars fty defs =
  let info = { fid; fty; fparams = tvars } in
  let env = defs.fun_env in
  { defs with fun_env = Env.add fid.Ident.id_str info env }

let add_fun env fid tvars fty = add_def (add_fun fid tvars fty) env

let add_ocaml_val vid tvars vty defs =
  let info = { fid = vid; fty = vty; fparams = tvars } in
  let env = defs.ocaml_val_env in
  { defs with ocaml_val_env = Env.add vid.Ident.id_str info env }

let add_ocaml_val env tvars id ty = add_def (add_ocaml_val tvars id ty) env

let add_mod mid mdefs defs =
  let menv = defs.mod_env in
  let info = { mid; mdefs } in
  { defs with mod_env = Env.add mid.Ident.id_str info menv }

let add_mod env mid mdefs = add_def (add_mod mid mdefs) env

(** [to_alias pty] Turns every instance of a type name application in [pty] into
    its respective alias, if it exists. *)
let rec to_alias = function
  | PTtyvar v -> PTtyvar v
  | PTtyapp (app, l) -> (
      match app.app_alias with
      | None -> PTtyapp (app, List.map to_alias l)
      | Some t -> t)
  | PTarrow (arg, res) -> PTarrow (to_alias arg, to_alias res)
  | PTtuple l -> PTtuple (List.map to_alias l)

let add_ocaml_type tid tparams tmut talias tmodel defs =
  let tenv = defs.ocaml_type_env in
  let info =
    { tid; tparams; tmut; talias = Option.map to_alias talias; tmodel }
  in
  { defs with ocaml_type_env = Env.add tid.Ident.id_str info tenv }

let add_ocaml_type env id params ~mut alias model =
  add_def (add_ocaml_type id params mut alias model) env

let add_gospel_type tid tparams talias defs =
  let tenv = defs.type_env in
  let info =
    {
      tid;
      tparams;
      tmut = false;
      talias = Option.map to_alias talias;
      tmodel = None;
    }
  in
  { defs with type_env = Env.add tid.Ident.id_str info tenv }

let add_gospel_type env id params alias =
  add_def (add_gospel_type id params alias) env

let add_record rid rparams rfields defs =
  let info = { rid; rparams; rfields } in
  (* Maps each record field name to the a field info object *)
  let f defs (rfid, rfty) =
    let rfenv = defs.field_env in
    let rfinfo = { rfid; rfty; rfrecord = info } in
    { defs with field_env = Env.add rfid.Ident.id_str rfinfo rfenv }
  in
  (* Map each field name to a [field_info] object. *)
  let defs = List.fold_left f defs rfields in
  (* Map the list of field names to the [record_info] object. *)
  let field_nms = List.map (fun (x, _) -> x.Ident.id_str) rfields in
  let env = defs.record_env in
  { defs with record_env = Record_env.add field_nms info env }

let add_record env rid rparams rfields =
  add_def (add_record rid rparams rfields) env

let add_exn id args env =
  let info = { eid = id; eargs = args } in
  { env with exn_env = Env.add id.id_str info env.exn_env }

let add_exn env id args = add_def (add_exn id args) env

(** [defs_union m1 m2] combines the Gospel definitions in [m1] and [m2]. In the
    case two definitions have the same name, we keep the value in [m2]. If the
    [ocaml] flag is true, then the OCaml definitions in [m2] are also added to
    the scope *)
let defs_union ~ocaml m1 m2 =
  let choose_snd = fun _ _ x -> Some x in
  let union e1 e2 = Env.union choose_snd e1 e2 in
  let runion e1 e2 = Record_env.union choose_snd e1 e2 in
  let ounion e1 e2 = if ocaml then Env.union choose_snd e1 e2 else e1 in
  {
    fun_env = union m1.fun_env m2.fun_env;
    type_env = union m1.type_env m2.type_env;
    field_env = union m1.field_env m2.field_env;
    record_env = runion m1.record_env m2.record_env;
    ocaml_type_env = ounion m1.ocaml_type_env m2.ocaml_type_env;
    ocaml_val_env = ounion m1.ocaml_val_env m2.ocaml_val_env;
    exn_env = ounion m1.exn_env m2.exn_env;
    mod_env = union m1.mod_env m2.mod_env;
  }

let local_open defs qid =
  let q, mods = access_mod defs qid in
  (q, defs_union ~ocaml:false defs mods)

let gospel_open env qid =
  let q, mods = access_mod env.scope qid in
  (q, { env with scope = defs_union ~ocaml:false env.scope mods })

(** [type_env] contains every primitive Gospel type. *)
let type_env =
  List.fold_left
    (fun tenv (x, y) ->
      Env.add x
        { tid = y; tparams = []; tmut = false; talias = None; tmodel = None }
        tenv)
    Env.empty Structure.primitive_list

(** [fun_env] contains the definitions for logical disjunction and conjunction.
    These are the only functions that exist as primitives. *)
let fun_env =
  let open Types in
  let op_ty = ty_arrow ty_prop (ty_arrow ty_prop ty_prop) in
  let op_info fid = { fid; fparams = []; fty = op_ty } in
  let conj = "/\\" in
  let disj = "\\/" in
  let conj_id = Ident.mk_id conj in
  let disj_id = Ident.mk_id disj in
  Env.empty |> Env.add conj (op_info conj_id) |> Env.add disj (op_info disj_id)

(** The empty environment. The only names that it contains with are primitive
    Gospel type definitions. *)
let empty_env =
  { defs = empty_defs; scope = { empty_defs with type_env; fun_env } }

let unit_id = Ident.mk_id "unit"

(** The initial environment for every Gospel file. The only names in scope are
    primitive Gospel and OCaml type definitions and the definitions within the
    Gospel standard library. *)
let init_env ?ocamlprimitives gospelstdlib =
  (* Combines two environments whose domains are disjoint. *)
  let union e1 e2 = Env.union (fun _ _ _ -> assert false) e1 e2 in

  (* Create a type environment that contains the primitive gospel types as well
     as the types defined in the standard library. *)
  let type_env = union type_env gospelstdlib.type_env in
  (* Creates a function enviornment that contains logical conjunction and
     disjunction as well as all values in the standard library. *)
  let fun_env = union fun_env gospelstdlib.fun_env in
  (* Create a module named [Gospelstdlib] with all the definitions within [stdlib]. *)
  let stdlib_info = { mid = Ident.stdlib_id; mdefs = gospelstdlib } in
  let mod_env =
    Env.add Ident.stdlib_id.id_str stdlib_info gospelstdlib.mod_env
  in
  let scope = { gospelstdlib with type_env; fun_env; mod_env } in
  (* If the environment with OCaml primitives is different from [None], add its
     definitions to the scope. *)
  let scope =
    match ocamlprimitives with
    | None -> scope
    | Some m ->
        (* Add unit to the set of OCaml primitives.  It is not defined
           alongside the other OCaml primitives because this type is
           needed for typechecking specifications, meaning its
           definition must be present at compile time. *)
        let unit_info =
          {
            tid = unit_id;
            tparams = [];
            tmut = false;
            talias = None;
            tmodel = None;
          }
        in
        let m =
          { m with ocaml_type_env = Env.add "unit" unit_info m.ocaml_type_env }
        in
        defs_union ~ocaml:true scope m
  in
  { defs = empty_defs; scope }
