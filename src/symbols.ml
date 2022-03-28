(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib
open Ttypes
module Ident = Identifier.Ident

(* Variable Symbols *)
type vsymbol = { vs_name : Ident.t; vs_ty : ty } [@@deriving show]

let create_vsymbol pid ty = { vs_name = Ident.of_preid pid; vs_ty = ty }

module Vs = struct
  type t = vsymbol

  let compare = Stdlib.compare
end

module Svs = Set.Make (Vs)
module Mvs = Map.Make (Vs)

(* Function and predicate symbols *)

type lsymbol = {
  ls_name : Ident.t;
  ls_args : ty list;
  ls_value : ty option;
  ls_constr : bool;
  (* true if it is a construct, false otherwise*)
  ls_field : bool; (* true if it is a record/model field *)
}
[@@deriving show]

(* CHECK *)
let ls_equal : lsymbol -> lsymbol -> bool = ( == )

module LS = struct
  type t = lsymbol

  let compare = Stdlib.compare
  let equal = ls_equal
  let hash = (Hashtbl.hash : lsymbol -> int)
end

module Sls = Set.Make (LS)
module Mls = Map.Make (LS)

let lsymbol ?(constr = false) ~field ls_name ls_args ls_value =
  { ls_name; ls_args; ls_value; ls_constr = constr; ls_field = field }

let fsymbol ?(constr = false) ~field nm tyl ty =
  lsymbol ~constr ~field nm tyl (Some ty)

let psymbol nm ty = lsymbol ~field:false nm ty None

let ls_subst_ts old_ts new_ts ({ ls_name; ls_constr; ls_field; _ } as ls) =
  let ls_args = List.map (ty_subst_ts old_ts new_ts) ls.ls_args in
  let ls_value = Option.map (ty_subst_ts old_ts new_ts) ls.ls_value in
  lsymbol ls_name ls_args ls_value ~constr:ls_constr ~field:ls_field

let ls_subst_ty old_ts new_ts new_ty ls =
  let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
  let ls_args = List.map subst ls.ls_args in
  let ls_value = Option.map subst ls.ls_value in
  lsymbol ls.ls_name ls_args ls_value ~constr:ls.ls_constr ~field:ls.ls_field

(** buil-in lsymbols *)

let ps_equ =
  let tv = fresh_ty_var "a" in
  psymbol Identifier.eq [ tv; tv ]

let fs_unit =
  fsymbol ~constr:true ~field:false
    (Ident.create ~loc:Location.none "unit")
    [] ty_unit

let fs_bool_true =
  fsymbol ~constr:true ~field:false
    (Ident.create ~loc:Location.none "True")
    [] ty_bool

let fs_bool_false =
  fsymbol ~constr:true ~field:false
    (Ident.create ~loc:Location.none "False")
    [] ty_bool

let fs_apply =
  let ty_a, ty_b = (fresh_ty_var "a", fresh_ty_var "b") in
  let ty_a_to_b = ty_app ts_arrow [ ty_a; ty_b ] in
  fsymbol ~field:false
    (Ident.create ~loc:Location.none "apply")
    [ ty_a_to_b; ty_a ] ty_b

(* CHECK do we need two hash tables? *)
let fs_tuple_ids = Hashtbl.create 17

let fs_tuple =
  let ls_tuples = Hashtbl.create 17 in
  fun n ->
    try Hashtbl.find ls_tuples n
    with Not_found ->
      let id = Ident.create ~loc:Location.none ("tuple" ^ string_of_int n) in
      let tyl = List.init n (fun _ -> fresh_ty_var "a") in
      let ty = ty_app (ts_tuple n) tyl in
      let ls = fsymbol ~constr:true ~field:false id tyl ty in
      Hashtbl.add fs_tuple_ids id ls;
      Hashtbl.add ls_tuples n ls;
      ls

let is_fs_tuple fs = fs.ls_constr = true && Hashtbl.mem fs_tuple_ids fs.ls_name
