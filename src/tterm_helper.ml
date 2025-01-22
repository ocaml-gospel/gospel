(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module W = Warnings
open Ppxlib
open Tterm
open Ttypes
open Symbols
module Ident = Identifier.Ident

(** type checking *)
let ls_arg_inst ls tl =
  let rec short_fold_left2 f accu l1 l2 =
    match (l1, l2) with
    | a1 :: l1, a2 :: l2 -> short_fold_left2 f (f accu a1 a2) l1 l2
    | _, _ -> accu
  in
  short_fold_left2
    (fun tvm ty t -> ty_match tvm ty t.t_ty)
    Mtv.empty (get_args ls) tl

let drop n xs =
  let rec aux n xs =
    match (n, xs) with
    | 0, xs -> xs
    | _, [] -> []
    | n, _ :: xs -> aux (n - 1) xs
  in
  if n < 0 then invalid_arg "drop" else aux n xs

let ls_app_inst ls tl ty =
  let s = ls_arg_inst ls tl in
  let vty = get_value ls in
  let vty =
    let ntl = List.length tl in
    if ntl >= List.length (get_args ls) then vty
    else
      (* build the result type in case of a partial application *)
      List.fold_right
        (fun t1 t2 -> { ty_node = Tyapp (ts_arrow, [ t1; t2 ]) })
        (drop ntl (get_args ls))
        vty
  in
  ty_match s vty ty

let rec p_vars p =
  match p.p_node with
  | Pwild | Pconst _ | Pinterval _ -> Svs.empty
  | Pvar vs -> Svs.singleton vs
  | Papp (_, pl) ->
      List.fold_left (fun vsl p -> Svs.union (p_vars p) vsl) Svs.empty pl
  | Por (p1, p2) -> Svs.union (p_vars p1) (p_vars p2)
  | Pas (p, vs) -> Svs.add vs (p_vars p)

let rec t_free_vars t =
  match t.t_node with
  | Tvar vs -> Svs.singleton vs
  | Tconst _ -> Svs.empty
  | Tapp (_, tl) ->
      List.fold_left (fun fvs t -> Svs.union (t_free_vars t) fvs) Svs.empty tl
  | Tfield (t, _) -> t_free_vars t
  | Tif (t1, t2, t3) ->
      Svs.union (t_free_vars t1) (Svs.union (t_free_vars t2) (t_free_vars t3))
  | Tlet (vs, t1, t2) ->
      let t1_fvs, t2_fvs = (t_free_vars t1, t_free_vars t2) in
      Svs.union t1_fvs (Svs.remove vs t2_fvs)
  | Tcase (t, pl) ->
      let t_fvs = t_free_vars t in
      let pl_fvs =
        List.fold_left
          (fun _ (p, _g, t) -> Svs.diff (t_free_vars t) (p_vars p))
          Svs.empty pl
      in
      Svs.union t_fvs pl_fvs
  | Tlambda (ps, t) ->
      let t_fvs = t_free_vars t in
      List.fold_right (fun p fvs -> Svs.diff fvs (p_vars p)) ps t_fvs
  | Tquant (_, vl, t) ->
      Svs.diff (t_free_vars t) (Svs.of_list (List.map (fun x -> x.bind_vs) vl))
  | Tbinop (_, t1, t2) -> Svs.union (t_free_vars t1) (t_free_vars t2)
  | Tnot t -> t_free_vars t
  | Told t -> t_free_vars t
  | Ttrue -> Svs.empty
  | Tfalse -> Svs.empty

(** Pattern constructors *)

let mk_pattern p_node p_ty p_loc = { p_node; p_ty; p_loc }
let p_wild ty = mk_pattern Pwild ty
let p_var vs = mk_pattern (Pvar vs) vs.vs_ty
let p_app ls pl ty = mk_pattern (Papp (ls, pl)) ty

(* CHECK ty matchs ls.ls_value *)
let p_or p1 p2 = mk_pattern (Por (p1, p2)) p1.p_ty

(* CHECK vars p1 = vars p2 *)
let p_as p vs = mk_pattern (Pas (p, vs)) p.p_ty
(* CHECK type vs = type p *)

let p_interval c1 c2 = mk_pattern (Pinterval (c1, c2)) ty_char

let p_const c =
  match c with
  | Pconst_integer _ -> mk_pattern (Pconst c) ty_int
  | Pconst_char _ -> mk_pattern (Pconst c) ty_char
  | Pconst_string _ -> mk_pattern (Pconst c) ty_string
  | Pconst_float _ -> mk_pattern (Pconst c) ty_float

(** Terms constructors *)

let mk_term t_node t_ty t_loc = { t_node; t_ty; t_attrs = []; t_loc }
let t_var vs = mk_term (Tvar vs) vs.vs_ty
let t_const c ty = mk_term (Tconst c) ty

let t_app ls tl ty loc =
  ignore (ls_app_inst ls tl ty : ty Mtv.t);
  mk_term (Tapp (ls, tl)) ty loc

let t_field t ls ty loc =
  ignore (ls_app_inst ls [ t ] ty : ty Mtv.t);
  mk_term (Tfield (t, ls)) ty loc

let t_if t1 t2 t3 = mk_term (Tif (t1, t2, t3)) t2.t_ty
let t_let vs t1 t2 = mk_term (Tlet (vs, t1, t2)) t2.t_ty

let t_case t1 ptl =
  match ptl with
  | [] -> assert false (* this is a syntax error *)
  | (_, _, t) :: _ -> mk_term (Tcase (t1, ptl)) t.t_ty

let t_quant q vsl t ty = mk_term (Tquant (q, vsl, t)) ty
let t_lambda ps t ty = mk_term (Tlambda (ps, t)) ty
let t_binop b t1 t2 = mk_term (Tbinop (b, t1, t2)) ty_bool
let t_not t = mk_term (Tnot t) ty_bool
let t_old t = mk_term (Told t) t.t_ty
let t_true = mk_term Ttrue ty_bool
let t_false = mk_term Tfalse ty_bool
let t_attr_set attr t = { t with t_attrs = attr }
let t_bool_true = mk_term (Tapp (fs_bool_true, [])) ty_bool
let t_bool_false = mk_term (Tapp (fs_bool_false, [])) ty_bool
let t_equ t1 t2 = t_app ps_equ [ t1; t2 ] ty_bool
let t_neq t1 t2 loc = t_not (t_equ t1 t2 loc)
let f_binop op f1 f2 = t_binop op f1 f2
let f_not f = t_not f
let f_and = f_binop Tand
let f_and_asym = f_binop Tand_asym
let f_or = f_binop Tor
let f_or_asym = f_binop Tor_asym
let f_implies = f_binop Timplies
let f_iff = f_binop Tiff
