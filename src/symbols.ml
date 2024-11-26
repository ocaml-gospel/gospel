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

let none_id = Ident.create ~loc:Location.none "()"
let none_vsymbol = { vs_name = none_id; vs_ty = ty_unit }

let create_vsymbol ?(path = []) pid ty =
  { vs_name = Ident.of_preid ~path pid; vs_ty = ty }

module Vs = struct
  type t = vsymbol

  let compare = Stdlib.compare
end

module Svs = Set.Make (Vs)
module Mvs = Map.Make (Vs)

(* Function, field and constructor symbols *)

type lsymbol =
  | Function_symbol of { ls_name : Ident.t; ls_args : ty list; ls_value : ty }
  | Constructor_symbol of {
      ls_name : Ident.t;
      ls_args : constructor_arguments;
      ls_value : ty;
    }
  | Field_symbol of { ls_name : Ident.t; ls_args : ty list; ls_value : ty }
[@@deriving show]

and constructor_arguments =
  | Cstr_tuple of ty list
  | Cstr_record of lsymbol list
[@@deriving show]

let function_symbol ls_name ls_args ls_value =
  Function_symbol { ls_name; ls_args; ls_value }

let constructor_symbol ls_name ls_args ls_value =
  Constructor_symbol { ls_name; ls_args; ls_value }

let field_symbol ls_name ls_args ls_value =
  Field_symbol { ls_name; ls_args; ls_value }

let get_name = function
  | Constructor_symbol { ls_name; _ }
  | Field_symbol { ls_name; _ }
  | Function_symbol { ls_name; _ } ->
      ls_name

let get_value = function
  | Constructor_symbol { ls_value; _ }
  | Field_symbol { ls_value; _ }
  | Function_symbol { ls_value; _ } ->
      ls_value

let get_args = function
  | Constructor_symbol { ls_args = Cstr_tuple tys; _ } -> tys
  | Constructor_symbol { ls_args = Cstr_record fields; _ } ->
      List.map get_value fields
  | Field_symbol { ls_args; _ } | Function_symbol { ls_args; _ } -> ls_args

(* We won't want to change the name of a symbol *)
let rec fmap f = function
  | Constructor_symbol { ls_name; ls_args = Cstr_tuple tys; ls_value } ->
      let ls_args = Cstr_tuple (List.map f tys) and ls_value = f ls_value in
      Constructor_symbol { ls_name; ls_args; ls_value }
  | Constructor_symbol { ls_name; ls_args = Cstr_record fields; ls_value } ->
      let ls_args = Cstr_record (List.map (fmap f) fields)
      and ls_value = f ls_value in
      Constructor_symbol { ls_name; ls_args; ls_value }
  | Field_symbol { ls_name; ls_args; ls_value } ->
      let ls_args = List.map f ls_args and ls_value = f ls_value in
      Field_symbol { ls_name; ls_args; ls_value }
  | Function_symbol { ls_name; ls_args; ls_value } ->
      let ls_args = List.map f ls_args and ls_value = f ls_value in
      Function_symbol { ls_name; ls_args; ls_value }

let ls_subst_ts old_ts new_ts symbol =
  let subst = ty_subst_ts old_ts new_ts in
  fmap subst symbol

let ls_subst_ty old_ts new_ts new_ty symbol =
  let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
  fmap subst symbol

(* CHECK *)
let ls_equal l r =
  let l = get_name l and r = get_name r in
  Ident.equal l r

module LS = struct
  type t = lsymbol

  let compare = Stdlib.compare
  let equal = ls_equal
  let hash = (Hashtbl.hash : lsymbol -> int)
end

module Sls = Set.Make (LS)
module Mls = Map.Make (LS)

(** built-in lsymbols *)

let ps_equ =
  let tv = fresh_ty_var "a" in
  function_symbol Identifier.eq [ tv; tv ] ty_bool

let fs_unit =
  constructor_symbol
    (Ident.create ~loc:Location.none "()")
    (Cstr_tuple []) ty_unit

let fs_bool_true =
  constructor_symbol
    (Ident.create ~loc:Location.none "true")
    (Cstr_tuple []) ty_bool

let fs_bool_false =
  constructor_symbol
    (Ident.create ~loc:Location.none "false")
    (Cstr_tuple []) ty_bool

let fs_apply =
  let ty_a, ty_b = (fresh_ty_var "a", fresh_ty_var "b") in
  let ty_a_to_b = ty_app ts_arrow [ ty_a; ty_b ] in
  function_symbol
    (Ident.create ~loc:Location.none "apply")
    [ ty_a_to_b; ty_a ] ty_b

let tvo = ts_option.ts_args |> function [ v ] -> v.tv_name | _ -> assert false
let tvl = ts_list.ts_args |> function [ v ] -> v.tv_name | _ -> assert false
let tv_option = { Ttypes.ty_node = Ttypes.Tyvar { Ttypes.tv_name = tvo } }
let tv_list = { Ttypes.ty_node = Ttypes.Tyvar { Ttypes.tv_name = tvl } }

let fs_option_none =
  constructor_symbol Identifier.none (Cstr_tuple []) (ty_option tv_option)

let fs_option_some =
  constructor_symbol Identifier.some (Cstr_tuple [ tv_option ])
    (ty_option tv_option)

let fs_list_nil =
  constructor_symbol Identifier.nil (Cstr_tuple []) (ty_list tv_list)

let fs_list_cons =
  constructor_symbol Identifier.cons
    (Cstr_tuple [ tv_list; ty_list tv_list ])
    (ty_list tv_list)

(* CHECK do we need two hash tables? *)
let fs_tuple_ids = Hashtbl.create 17

let fs_tuple =
  let ls_tuples = Hashtbl.create 17 in
  fun n ->
    try Hashtbl.find ls_tuples n
    with Not_found ->
      let id = Ident.create ~loc:Location.none ("tuple" ^ string_of_int n) in
      let ts = ts_tuple n in
      let tyl = List.map ty_of_var ts.ts_args in
      let ty = ty_app (ts_tuple n) tyl in
      let ls = constructor_symbol id (Cstr_tuple tyl) ty in
      Hashtbl.add fs_tuple_ids id ls;
      Hashtbl.add ls_tuples n ls;
      ls

let is_fs_tuple = function
  | Constructor_symbol { ls_name; _ } -> Hashtbl.mem fs_tuple_ids ls_name
  | _ -> false
