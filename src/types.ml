(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Itypes

type tyvar = int

let inject n = n

type 'a structure = 'a S.structure

let pprint = S.pprint

type ty =
  | Tyvar of tyvar  (** Type variables (e.g. 'a, 'b). *)
  | Tyapp of Ident.t * ty list  (** Decoded types.*)
  | Tyarrow of ty * ty  (** Function types *)

(** Maps a type variable to a decoded type*)
let variable x = Tyvar x

(** Maps a structure whose variables are decoded types into a decoded type *)
let structure t =
  match t with
  | S.Tyapp (id, l) -> Tyapp (id, l)
  | Tyarrow (t1, t2) -> Tyarrow (t1, t2)

(** Since Gospel types are not allowed to be cyclic, we do not need to define
    the mu function *)
let mu _ _ = assert false

open Utils.Fmt

let rec print_tv fmt tv = pp fmt "%d" tv
and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_ty fmt = function
  | Tyvar v -> pp fmt "%a" print_tv v
  | Tyarrow (ty1, ty2) -> print_arrow_ty fmt [ ty1; ty2 ]
  | Tyapp (ts, []) -> pp fmt "%s" ts.id_str
  | Tyapp (ts, [ ty ]) -> pp fmt "%a %s" print_ty ty ts.id_str
  | Tyapp (ts, tyl) -> pp fmt "(%a) %s" (list ~sep:comma print_ty) tyl ts.id_str
