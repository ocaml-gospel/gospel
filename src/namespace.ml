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

let find_fun ~ocaml = fun d -> if ocaml then d.ocaml_val_env else d.fun_env
let find_type ~ocaml = fun d -> if ocaml then d.ocaml_type_env else d.type_env
let find_exn = fun d -> d.exn_env
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

let type_info ~ocaml =
  unique_toplevel_qualid
    (fun x -> x.tid)
    (find_type ~ocaml)
    (fun id -> W.Unbound_type id)

let exn_info =
  unique_toplevel_qualid
    (fun x -> x.eid)
    find_exn
    (fun id -> W.Unbound_exception id)

let get_exn_info env id =
  let id, info = exn_info env id in
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
  let q, info = type_info ~ocaml env q in
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

let fun_info ~ocaml =
  unique_toplevel_qualid
    (fun x -> x.fid)
    (find_fun ~ocaml)
    (fun id -> W.Unbound_variable id)

let ocaml_val_check ocaml_vals env q =
  (* If there are no valid ocaml values, then no lookup is performed. *)
  if Tbl.length ocaml_vals = 0 then None
  else
    try
      (* Raises [W.Error] if [q] is not in the OCaml namespace. *)
      let q, _ = fun_info ~ocaml:true env q in
      let pty = Tbl.find ocaml_vals (Uast_utils.leaf q).id_tag in
      Some (q, pty)
    with W.Error _ -> None

let fun_qualid ocaml_vals env q =
  match ocaml_val_check ocaml_vals env q with
  | None ->
      let q, info = fun_info ~ocaml:false env q in
      (q, info.fparams, info.fty)
  | Some (q, ty_gospel) -> (q, [], ty_gospel)

let ocaml_val_qualid env q =
  let q, info = fun_info ~ocaml:true env q in
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
  let info = { fid; fty; fparams = get_vars fty } in
  let env = defs.fun_env in
  { defs with fun_env = Env.add fid.Ident.id_str info env }

let add_fun env fty fid = add_def (add_fun fty fid) env

let add_ocaml_val vid vty defs =
  let info = { fid = vid; fty = vty; fparams = get_vars vty } in
  let env = defs.ocaml_val_env in
  { defs with ocaml_val_env = Env.add vid.Ident.id_str info env }

let add_ocaml_val env id ty = add_def (add_ocaml_val id ty) env

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
  let conj = "infix /\\" in
  let disj = "infix \\/" in
  let conj_id = Ident.mk_id conj Location.none in
  let disj_id = Ident.mk_id disj Location.none in
  Env.empty |> Env.add conj (op_info conj_id) |> Env.add disj (op_info disj_id)

(** The empty environment. The only names that it contains with are primitive
    Gospel type definitions. *)
let empty_env =
  { defs = empty_defs; scope = { empty_defs with type_env; fun_env } }

let unit_id = Ident.mk_id "unit" Location.none

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
