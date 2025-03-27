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
module S = Structure

type tyvar = int

(** [leaf q] Receives an identifier of the form [M1.M2.M....id] and returns [id]
*)
let leaf q = Uast_utils.leaf q

let inject n = n

type 'a structure = 'a S.structure

let pprint = S.pprint

type ty = Id_uast.pty

(** Maps a type variable to a decoded type*)
let variable _ = PTtyvar (Ident.tvar ())

let mk_info ?(alias = None) id = { Id_uast.app_qid = id; app_alias = alias }

(** Maps a structure whose variables are decoded types into a decoded type *)
let structure t =
  match t with
  | S.Tyapp (id, l) ->
      let info = mk_info id in
      PTtyapp (info, l)
  | Tyarrow (t1, t2) -> PTarrow (t1, t2)
  | Tytuple l -> PTtuple l
  | Tvar v -> PTtyvar v

(** Since Gospel types are not allowed to be cyclic, the only time [mu v ty] is
    called is when the Inferno solver raises a [Cycle] exception. Since this
    function is only used for error reporting, this function will return a
    [Tyarrow] where the argument is [Tyvar v] and the result type is [ty]. *)
let mu _ ty = PTarrow (PTtyvar (Ident.tvar ()), ty)

open Utils.Fmt

let ty_arrow arg ret = PTarrow (arg, ret)
let ty_prop = PTtyapp (mk_info (Qid S.prop_id), [])

let rec print_tv fmt tv = pp fmt "'%s" tv.Ident.id_str
and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_ty fmt = function
  | PTtyvar v -> pp fmt "%a" print_tv v
  | PTarrow ((PTarrow _ as ty1), ty2) ->
      pp fmt "@[%a@]@[%a@]@[%a@]" (parens print_ty) ty1 arrow () print_ty ty2
  | PTarrow (ty1, ty2) -> print_arrow_ty fmt [ ty1; ty2 ]
  | PTtyapp (ts, []) -> pp fmt "@[%s@]" (leaf ts.app_qid).id_str
  | PTtyapp (ts, [ ty ]) ->
      pp fmt "@[%a@] %s" print_ty ty (leaf ts.app_qid).id_str
  | PTtyapp (ts, tyl) ->
      pp fmt "@[%a@] %s"
        (parens (list ~sep:comma print_ty))
        tyl (leaf ts.app_qid).id_str
  | PTtuple l -> pp fmt "@[%a@]" (list ~sep:star print_tuple_par) l

and print_tuple_par fmt ty =
  match ty with
  | PTtuple _ | PTarrow _ -> pp fmt "@[%a@]" (parens print_ty) ty
  | _ -> print_ty fmt ty

let expand_alias t =
  match t with
  | PTtyapp (app, _) -> Option.value ~default:t app.app_alias
  | _ -> t

(** [incompatible ~shallow t1 t2] when [shallow] is [false] returns the
    fragments of the type expressions [t1] and [t2] that are incompatible with
    one another. If [shallow] is [true], this function will not check any of the
    sub expressions in either type and return [None] if the "top" most types are
    compatible with each other, e.g. it would return [None] if [t1 = int bag]
    and [t2 = string bag] and [shallow] is [true] since they are both
    applications of [bag], although the type parameters are different.

    Note: [incompatible] should not be used outside of generating error
    messages. More specifically, this function should not be used for
    typechecking. *)
let rec incompatible ~shallow t1 t2 =
  (* When the [shallow] flag is set to [true], this function compares the first
     element of [l1] with the first element [l2], the second of [l1] with the
     second of [l2] and so on until it finds an incompatible type.

     Assumption: [l1] and [l2] have the same lengths *)
  let traverse l1 l2 =
    if shallow then None
    else
      List.find_map
        (fun (t1, t2) -> incompatible ~shallow t1 t2)
        (List.combine l1 l2)
  in
  match (expand_alias t1, expand_alias t2) with
  | PTtyapp (app1, l1), PTtyapp (app2, l2) ->
      if leaf app1.app_qid = leaf app2.app_qid then traverse l1 l2
      else Some (t1, t2)
  | PTtyvar v1, PTtyvar v2 -> if Ident.equal v1 v2 then None else Some (t1, t2)
  | PTarrow (arg1, ret1), PTarrow (arg2, ret2) ->
      traverse [ arg1; ret1 ] [ arg2; ret2 ]
  | PTtuple l1, PTtuple l2 when List.compare_lengths l1 l2 = 0 -> traverse l1 l2
  | _ -> Some (t1, t2)

(** [shallow_incompatible ty1 ty2] checks if the "top" most types of [ty1] and
    [ty2] are compatible with one another. For more details, check the comment
    for [incompatible]. *)
let shallow_incompatible ty1 ty2 =
  Option.is_some (incompatible ~shallow:true ty1 ty2)

(** [deep_incompatible ty1 ty2] returns the sub expressions of [ty1] and [ty2]
    that are incompatible with one another.

    Assumption: [ty1] and [ty2] are incompatible *)
let deep_incompatible ty1 ty2 = Option.get (incompatible ~shallow:false ty1 ty2)

(** [n_args ty] receives a type and returns the amount of arguments its
    inhabitants expect. If [ty] is a non_functional type, this function returns
    [0]. *)
let rec n_args t =
  match expand_alias t with PTarrow (_, ret) -> 1 + n_args ret | _ -> 0

let incompatible_types loc ty1 ty2 =
  let module W = Warnings in
  let n_args1 = n_args ty1 in
  let n_args2 = n_args ty2 in
  let ty1s = Fmt.str "%a" print_ty ty1 in
  let ty2s = Fmt.str "%a" print_ty ty2 in
  let loc = Uast_utils.mk_loc loc in
  let error =
    if n_args1 <> n_args2 && n_args1 <> 0 && n_args2 <> 0 then
      W.Arity_mismatch (ty1s, ty2s, n_args1, n_args2)
    else if shallow_incompatible ty1 ty2 then W.Bad_type (ty1s, ty2s)
    else
      let ty1_sub, ty2_sub = deep_incompatible ty1 ty2 in
      let ty1_frags = Fmt.str "%a" print_ty ty1_sub in
      let ty2_frags = Fmt.str "%a" print_ty ty2_sub in
      W.Bad_subtype (ty1s, ty2s, ty1_frags, ty2_frags)
  in
  W.error error ~loc

let cycle loc t =
  let module W = Warnings in
  let loc = Uast_utils.mk_loc loc in
  match t with
  | PTarrow (PTtyvar v, t) ->
      let tys = Fmt.str "%a" print_ty t in
      W.error ~loc (W.Cycle (v.id_str, tys))
  | _ -> assert false (* See comment for [mu] function *)
