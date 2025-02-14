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
let leaf q = match q with Qid id -> id | Qdot (_, id) -> id

let inject n = n

type 'a structure = 'a S.structure

let pprint = S.pprint

type ty = Id_uast.pty

(** Maps a type variable to a decoded type*)
let variable _ = PTtyvar (Ident.tvar ())

(** Maps a structure whose variables are decoded types into a decoded type *)
let structure t =
  match t with
  | S.Tyapp (id, l) -> PTtyapp (Qid id, l)
  | Tyarrow (t1, t2) -> PTarrow (t1, t2)

(** Since Gospel types are not allowed to be cyclic, we do not need to define
    the mu function *)
let mu _ _ = assert false

open Utils.Fmt

let ty_arrow arg ret = PTarrow (arg, ret)
let ty_bool = PTtyapp (Qid S.bool_id, [])

let rec print_tv fmt tv = pp fmt "'%s" tv.Ident.id_str
and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_ty fmt = function
  | PTtyvar v -> pp fmt "%a" print_tv v
  | PTarrow ((PTarrow _ as ty1), ty2) ->
      pp fmt "@[%a@]@[%a@]@[%a@]" (parens print_ty) ty1 arrow () print_ty ty2
  | PTarrow (ty1, ty2) -> print_arrow_ty fmt [ ty1; ty2 ]
  | PTtyapp (ts, []) -> pp fmt "@[%s@]" (leaf ts).id_str
  | PTtyapp (ts, [ ty ]) -> pp fmt "%a %s" print_ty ty (leaf ts).id_str
  | PTtyapp (ts, tyl) ->
      pp fmt "(%a) %s" (list ~sep:comma print_ty) tyl (leaf ts).id_str
  | PTtuple _ -> assert false
