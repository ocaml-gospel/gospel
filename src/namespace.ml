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
  talias : Id_uast.pty option;
      (* In case of a type declaration of the form [type t = alias] where alias is
     some type expression, this value is [Some alias]. All applications of type
     [tid] are replaced with [talias] during typechecking.

     The [talias] field always points to the "top most" alias. This means if we
     have the following program [type t1 type t2 = t1 type t3 = t2] the [talias]
     field for [t3] will be [t1].*)
}

type record_info = {
  rid : Ident.t; (* The name of the record type. *)
  rparams : Ident.t list;
  (* The type parameters for the record type. Should be equal to the [tparams]
       list in the entry for [rid] in the corresponding type environment. *)
  rfields : (Ident.t * Id_uast.pty) list; (* The list of all record fields. *)
}

type field_info = { rfid : Ident.t; rfty : Id_uast.pty; rfrecord : record_info }

type mod_info = { mid : Ident.t; mdefs : mod_defs }

and mod_defs = {
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
  mod_env : mod_info Env.t; (* Nested modules *)
}
(** Set of top level module definitions *)

let empty_defs =
  {
    fun_env = Env.empty;
    type_env = Env.empty;
    field_env = Env.empty;
    record_env = Record_env.empty;
    mod_env = Env.empty;
  }

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
let find_fields = fun d -> d.field_env

(** [access_mod defs q] returns a qualified identifer where the identifiers used
    in [q] have been replaced with uniquely tagged identifiers. This function
    assumes that all the identifiers in [q] are belong to the module namespace.
    This function also returns the definitions contained in the module
    associated with [q]. *)
let rec access_mod defs =
  (* Looks up a module identifier and throws an error in case it is not in
     scope. *)
  let lookup defs pid =
    try lookup find_mod defs pid
    with Not_found -> W.error ~loc:pid.pid_loc (W.Unbound_module pid.pid_str)
  in
  function
  | Parse_uast.Qid pid ->
      let info = lookup defs pid in
      let id = info.mid in
      (Qid id, info.mdefs)
  | Qdot (q, pid) ->
      let q, defs = access_mod defs q in
      let info = lookup defs pid in
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

(** [unique_toplevel_qualid field env err defs q] returns the information
    associated with identifier [q] in the environment [env defs] and returns [q]
    as a fully resolved identifier. This identifier is built by applying [field]
    to the [info] object associated with [q]. If no identifier is found with
    this name, we call the [err] function to produce an appropriate error. *)
let unique_toplevel_qualid field env err defs q =
  try
    let q, info = unique_toplevel env defs q in
    let id = field info in
    (mk_qid q id, info)
  with Not_found ->
    let id = Uast_utils.flatten q in
    let loc = match q with Qid id | Qdot (_, id) -> id.pid_loc in
    W.error ~loc (err id)

let type_info =
  unique_toplevel_qualid
    (fun x -> x.tid)
    find_type
    (fun id -> W.Unbound_type id)

module M = Map.Make (Int)

(** [build_alias var_map alias] returns a new type with the same structure as
    [alias] where every type variable has been replaced with their binding in
    [var_map]. *)
let rec build_alias var_map alias =
  let build_alias = build_alias var_map in
  match alias with
  | PTtyvar id -> M.find id.id_tag var_map
  | PTarrow (t1, t2) -> PTarrow (build_alias t1, build_alias t2)
  | PTtyapp (q, l) -> PTtyapp (q, List.map build_alias l)
  | PTtuple l -> PTtuple (List.map build_alias l)

let resolve_alias env q l =
  let q, info = type_info env q in
  let params = info.tparams in
  let len1, len2 = (List.length params, List.length l) in
  if len1 <> len2 then (* type arity check *)
    W.error ~loc:Location.none (W.Bad_arity (info.tid.Ident.id_str, len1, len2));
  let alias =
    match info.talias with
    | None ->
        (* If this type has no alias, apply the type identifier to the list of
        arguments. *)
        None
    | Some alias ->
        (* When there is an alias, we create a map that binds each type variable
        identifier in [alias] to the corresponding type in [l]. *)
        let var_map =
          List.fold_left2
            (fun acc avar tvar -> M.add avar.Ident.id_tag tvar acc)
            M.empty params l
        in
        Some (build_alias var_map alias)
  in
  PTtyapp (Types.mk_info q ~alias, l)

let fun_info =
  unique_toplevel_qualid
    (fun x -> x.fid)
    find_fun
    (fun id -> W.Unbound_variable id)

let fun_qualid env q =
  let q, info = fun_info env q in
  (q, info.fparams, info.fty)

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

let get_field_info defs q =
  let q, info =
    unique_toplevel_qualid
      (fun x -> x.rfid)
      find_fields
      (fun id -> Warnings.Unbound_record_label id)
      defs q
  in
  (q, info.rfty, { params = info.rfrecord.rparams; name = info.rfrecord.rid })

(** [get_vars ty] returns the type variables within the type [ty]. *)
let get_vars ty =
  let tbl = Hashtbl.create 100 in
  let rec get_vars = function
    | PTtyvar id -> Hashtbl.add tbl id.id_tag id
    | PTtyapp (_, l) -> List.iter get_vars l
    | PTarrow (arg, ret) ->
        get_vars arg;
        get_vars ret
    | PTtuple l -> List.iter get_vars l
  in
  get_vars ty;
  Hashtbl.to_seq_values tbl |> List.of_seq

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

let add_fun fid fty defs =
  let env = defs.fun_env in
  let info = { fid; fty; fparams = get_vars fty } in
  { defs with fun_env = Env.add fid.Ident.id_str info env }

let add_fun env fty fid = add_def (add_fun fty fid) env

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

let add_type tid tparams talias defs =
  let tenv = defs.type_env in
  let info = { tid; tparams; talias = Option.map to_alias talias } in
  { defs with type_env = Env.add tid.Ident.id_str info tenv }

let add_type env tid tparams talias = add_def (add_type tid tparams talias) env

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

(** [type_env] contains every primitive Gospel type. *)
let type_env =
  List.fold_left
    (fun tenv (x, y) -> Env.add x { tid = y; tparams = []; talias = None } tenv)
    Env.empty Structure.primitive_list

(** [fun_env] contains the definitions for logical disjunction and conjunction.
    These are the only functions that exist as primitives. *)
let fun_env =
  let open Types in
  let op_ty = ty_arrow ty_bool (ty_arrow ty_bool ty_bool) in
  let op_info fid = { fid; fparams = []; fty = op_ty } in
  let conj = "infix /\\" in
  let disj = "infix \\/" in
  let conj_id = Ident.mk_id conj Location.none in
  let disj_id = Ident.mk_id disj Location.none in
  Env.empty |> Env.add conj (op_info conj_id) |> Env.add disj (op_info disj_id)

(** The empty environment. The only names that it contains with are primitive
    Gospel type definitions. *)
let empty_env =
  { defs = empty_defs; scope = { empty_defs with type_env; fun_env } }

(** The initial environment for every Gospel file. The only names that it
    contains with are primitive Gospel type definitions and the definitions
    within the Gospel standard library. *)
let init_env stdlib =
  (* Combines two environments whose domains are disjoint. *)
  let union e1 e2 = Env.union (fun _ _ _ -> assert false) e1 e2 in

  (* Create a type environment that contains the primitive gospel types as well
     as the types defined in the standard library. *)
  let type_env = union type_env stdlib.type_env in
  (* Creates a function enviornment that contains logical conjunction and
     disjunction as well as all values in the standard library. *)
  let fun_env = union fun_env stdlib.fun_env in
  (* Create a module named [Gospelstdlib] with all the definitions within [stdlib]. *)
  let stdlib_info = { mid = Ident.stdlib_id; mdefs = stdlib } in
  let mod_env = Env.add Ident.stdlib_id.id_str stdlib_info stdlib.mod_env in
  { defs = empty_defs; scope = { stdlib with type_env; fun_env; mod_env } }
