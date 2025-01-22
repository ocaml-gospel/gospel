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
open Utils
module Ident = Identifier.Ident

(** type variables *)

type tvsymbol = { tv_name : Ident.t } [@@deriving show]

let tv_equal x y = Ident.equal x.tv_name y.tv_name

module Tvar = struct
  type t = tvsymbol

  let equal = tv_equal
  let compare x y = Ident.compare x.tv_name y.tv_name
  let hash tv = Ident.hash tv.tv_name
end

module Htv = Hashtbl.Make (Tvar)
module Mtv = Map.Make (Tvar)

let create_tv id = { tv_name = id }
let fresh_tv ?(loc = Location.none) s = { tv_name = Ident.create ~loc s }

let tv_of_string =
  let hs = Hashtbl.create 0 in
  fun ?(loc = Location.none) s ->
    try Hashtbl.find hs s
    with Not_found ->
      let tv = create_tv (Ident.create ~loc s) in
      Hashtbl.add hs s tv;
      tv

(** types *)

type ty = { ty_node : ty_node } [@@deriving show]

and ty_node = Tyvar of tvsymbol | Tyapp of tysymbol * ty list
[@@deriving show]

and tysymbol = {
  ts_ident : Ident.t;
  ts_args : tvsymbol list;
  (* we need to keep variables to do things like
     type ('a,'b) t1  type ('a,'b) t2 = ('b,'a) t1 *)
  ts_alias : ty option;
  ts_model : bool * model;
}
[@@deriving show]

and model = Fields | Self | Model of ty [@@deriving show]

let ts_equal x y = Ident.equal x.ts_ident y.ts_ident

let rec ty_equal x y =
  match (x.ty_node, y.ty_node) with
  | Tyvar tvx, Tyvar tvy -> tv_equal tvx tvy
  | Tyapp (tsx, tylx), Tyapp (tsy, tyly) ->
      ts_equal tsx tsy && List.for_all2 ty_equal tylx tyly
  | _ -> false

module Ts = struct
  type t = tysymbol

  let equal = ts_equal
  let hash x = x.ts_ident.id_tag
  let compare x y = Ident.compare x.ts_ident y.ts_ident
end

module Mts = Map.Make (Ts)
module Hts = Hashtbl.Make (Ts)

let ts id args =
  { ts_ident = id; ts_args = args; ts_alias = None; ts_model = (false, Self) }

let mk_ts id args alias model =
  { ts_ident = id; ts_args = args; ts_alias = alias; ts_model = model }

let ts_ident ts = ts.ts_ident
let ts_args ts = ts.ts_args
let ts_alias ts = ts.ts_alias
let ts_arity ts = List.length ts.ts_args

let fresh_ty_var ?(loc = Location.none) s =
  { ty_node = Tyvar { tv_name = Ident.create ~loc s } }

let ty_of_var tv = { ty_node = Tyvar tv }

(** smart constructors & utils *)

let ty_app ?loc ts tyl =
  if ts_arity ts = List.length tyl then { ty_node = Tyapp (ts, tyl) }
  else
    let loc = Option.value ~default:ts.ts_ident.id_loc loc in
    W.error ~loc
      (W.Bad_type_arity (ts.ts_ident.id_str, ts_arity ts, List.length tyl))

let ts_loc_sp = ts (Ident.create ~loc:Location.none "loc") []
let ty_loc_sp = ty_app ts_loc_sp []

let ts_loc =
  ts (Ident.create ~loc:Location.none "loc") [ fresh_tv ~loc:Location.none "a" ]

let ty_loc ty = ty_app ts_loc [ ty ]

let rec ty_apply_lens ty lens =
  match (ty.ty_node, lens.ty_node) with
  | Tyvar v1, Tyvar v2 when tv_equal v1 v2 -> ty
  | Tyapp (ts1, l1), Tyapp (ts2, l2) when ts_equal ts1 ts2 -> (
      match ts1.ts_model with
      | _, Model ty -> ty
      | _ -> { ty_node = Tyapp (ts1, List.map2 ty_apply_lens l1 l2) })
  | Tyapp (ts1, _), Tyapp (ts2, _) when ts_equal ts2 ts_loc_sp ->
      if fst ts1.ts_model then ty_loc ty
      else assert false (*TODO: replace with W.error *)
  | _ -> assert false (*TODO: replace with W.error *)

let rec ty_full_inst ?loc m ty =
  match ty.ty_node with
  | Tyvar tv -> Mtv.find tv m
  | Tyapp (ts, tyl) -> ty_app ?loc ts (List.map (ty_full_inst m) tyl)

let ts_match_args ?loc ts tl =
  try List.fold_right2 Mtv.add ts.ts_args tl Mtv.empty
  with Invalid_argument _ ->
    let loc = Option.value ~default:ts.ts_ident.id_loc loc in
    W.error ~loc
      (W.Bad_type_arity (ts.ts_ident.id_str, ts_arity ts, List.length tl))

let ty_app ?loc ts tyl =
  match ts.ts_alias with
  | None -> ty_app ?loc ts tyl
  | Some ty -> ty_full_inst ?loc (ts_match_args ?loc ts tyl) ty

let rec ts_subst_ts old_ts new_ts
    ({ ts_ident; ts_args; ts_alias; ts_model } as ts) =
  if ts_equal old_ts ts then new_ts
  else
    let ts_alias = Option.map (ty_subst_ts old_ts new_ts) ts_alias in
    mk_ts ts_ident ts_args ts_alias ts_model

and ty_subst_ts old_ts new_ts ty =
  match ty.ty_node with
  | Tyvar _ -> ty
  | Tyapp (ts, tyl) ->
      let ts = if ts_equal old_ts ts then new_ts else ts in
      ty_app ts (List.map (ty_subst_ts old_ts new_ts) tyl)

let rec ty_subst_ty old_ts new_ts new_ty ty =
  match ty.ty_node with
  | Tyvar _ -> ty
  | Tyapp (ts, tyl) ->
      if ts_equal old_ts ts then ty_full_inst (ts_match_args new_ts tyl) new_ty
      else
        let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
        let tyl = List.map subst tyl in
        ty_app ts tyl

and ts_subst_ty old_ts new_ts new_ty ts =
  let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
  let ts_alias = Option.map subst ts.ts_alias in
  mk_ts ts.ts_ident ts.ts_args ts_alias new_ts.ts_model

(** type matching *)

(* if possible returns the map that allows to instanciate ty1 with ty2 *)
let rec ty_match mtv ty1 ty2 =
  let set = function
    | None -> Some ty2
    | Some ty1 as r when ty_equal ty1 ty2 -> r
    | _ -> raise Exit
  in
  match (ty1.ty_node, ty2.ty_node) with
  | Tyvar tv1, _ -> Mtv.update tv1 set mtv
  | Tyapp (ts1, tyl1), Tyapp (ts2, tyl2) when ts_equal ts1 ts2 ->
      List.fold_left2 ty_match mtv tyl1 tyl2
  | _ -> raise Exit

exception TypeMismatch of ty * ty

let ty_match mtv ty1 ty2 =
  let rec ty_inst mtv ty =
    match ty.ty_node with
    | Tyvar n -> ( try Mtv.find n mtv with Not_found -> ty)
    | Tyapp (ts, tyl) -> { ty_node = Tyapp (ts, List.map (ty_inst mtv) tyl) }
  in
  try ty_match mtv ty1 ty2
  with Exit -> raise (TypeMismatch (ty_inst mtv ty1, ty2))

let ty_equal_check ty1 ty2 =
  if not (ty_equal ty1 ty2) then raise (TypeMismatch (ty1, ty2))

(** Built-in symbols *)

let ts_unit = ts (Ident.create ~loc:Location.none "unit") []
let ts_integer = ts (Ident.create ~loc:Location.none "integer") []
let ts_int = ts (Ident.create ~loc:Location.none "int") []
let ts_char = ts (Ident.create ~loc:Location.none "char") []
let ts_bytes = ts (Ident.create ~loc:Location.none "bytes") []
let ts_string = ts (Ident.create ~loc:Location.none "string") []
let ts_float = ts (Ident.create ~loc:Location.none "float") []
let ts_bool = ts (Ident.create ~loc:Location.none "bool") []
let ts_exn = ts (Ident.create ~loc:Location.none "exn") []

let ts_array =
  ts
    (Ident.create ~loc:Location.none "array")
    [ fresh_tv ~loc:Location.none "a" ]

let ts_list =
  ts
    (Ident.create ~loc:Location.none "list")
    [ fresh_tv ~loc:Location.none "a" ]

let ts_option =
  ts
    (Ident.create ~loc:Location.none "option")
    [ fresh_tv ~loc:Location.none "a" ]

let ts_int32 = ts (Ident.create ~loc:Location.none "int32") []
let ts_int64 = ts (Ident.create ~loc:Location.none "int64") []
let ts_nativeint = ts (Ident.create ~loc:Location.none "nativeint") []

let ts_format6 =
  ts
    (Ident.create ~loc:Location.none "format6")
    [
      fresh_tv ~loc:Location.none "a";
      fresh_tv ~loc:Location.none "b";
      fresh_tv ~loc:Location.none "c";
      fresh_tv ~loc:Location.none "d";
      fresh_tv ~loc:Location.none "e";
      fresh_tv ~loc:Location.none "f";
    ]

let ts_lazy =
  ts
    (Ident.create ~loc:Location.none "lazy")
    [ fresh_tv ~loc:Location.none "a" ]

let ts_tuple =
  let ts_tuples = Hashtbl.create 0 in
  Hashtbl.add ts_tuples 0 ts_unit;
  fun n ->
    try Hashtbl.find ts_tuples n
    with Not_found ->
      let ts_id = Ident.create ~loc:Location.none ("tuple" ^ string_of_int n) in
      let ts_args =
        List.init n (fun x ->
            fresh_tv ~loc:Location.none ("a" ^ string_of_int x))
      in
      let ts = ts ts_id ts_args in
      Hashtbl.add ts_tuples n ts;
      ts

let ts_arrow =
  let ta = fresh_tv ~loc:Location.none "a" in
  let tb = fresh_tv ~loc:Location.none "b" in
  let id = Ident.create ~loc:Location.none "->" in
  ts id [ ta; tb ]

let is_ts_tuple ts =
  let n = ts_arity ts in
  n > 0
  &&
  let ts_tuple = ts_tuple n in
  Ident.equal ts_tuple.ts_ident ts.ts_ident

let is_ts_arrow ts = Ident.equal ts_arrow.ts_ident ts.ts_ident
let ty_unit = ty_app ts_unit []
let ty_integer = ty_app ts_integer []
let ty_int = ty_app ts_int []
let ty_bool = ty_app ts_bool []
let ty_float = ty_app ts_float []
let ty_char = ty_app ts_char []
let ty_string = ty_app ts_string []
let ty_option ty = ty_app ts_option [ ty ]
let ty_list ty = ty_app ts_list [ ty ]

let ty_tuple = function
  | [] -> ty_unit
  | [ ty ] -> ty
  | tyl -> ty_app (ts_tuple (List.length tyl)) tyl

type exn_type =
  | Exn_tuple of ty list
  (* exception E of int * int
       -> Exn_tuple [int_ty;int_ty]
     exception E of (int*int)
       -> Exn_tuple [Tyapp (ts_tuple 2) [ty_int;ty_int]] *)
  | Exn_record of (Ident.t * ty) list
[@@deriving show]

type xsymbol = { xs_ident : Ident.t; xs_type : exn_type } [@@deriving show]

let xsymbol id ty = { xs_ident = id; xs_type = ty }
let xs_equal x y = Ident.equal x.xs_ident y.xs_ident

module Xs = struct
  type t = xsymbol

  let equal = xs_equal
  let compare x y = Ident.compare x.xs_ident y.xs_ident
end

module Mxs = Map.Make (Xs)

let xs_subst_ts old_ts new_ts { xs_ident; xs_type } =
  let subst = function
    | Exn_tuple tyl -> Exn_tuple (List.map (ty_subst_ts old_ts new_ts) tyl)
    | Exn_record l ->
        Exn_record
          (List.map (fun (id, ty) -> (id, ty_subst_ts old_ts new_ts ty)) l)
  in
  xsymbol xs_ident (subst xs_type)

let xs_subst_ty old_ts new_ts new_ty xs =
  let subst = function
    | Exn_tuple tyl ->
        let subst ty = ty_subst_ty old_ts new_ts new_ty ty in
        Exn_tuple (List.map subst tyl)
    | Exn_record l ->
        let subst (id, ty) = (id, ty_subst_ty old_ts new_ts new_ty ty) in
        Exn_record (List.map subst l)
  in
  { xs with xs_type = subst xs.xs_type }

(** Pretty printers *)

open Fmt

let print_tv fmt tv =
  pp fmt
    (if tv.tv_name.id_str = "_" then "%a" else "'%a")
    Ident.pp_simpl tv.tv_name

let print_ts_name fmt ts = pp fmt "@[%a@]" Ident.pp_simpl (ts_ident ts)

let rec print_ty fmt { ty_node } = print_ty_node fmt ty_node
and print_arrow_ty fmt = list ~sep:arrow print_ty fmt

and print_ty_node fmt = function
  | Tyvar v -> pp fmt "%a" print_tv v
  | Tyapp (ts, []) -> print_ts_name fmt ts
  | Tyapp (ts, tys) when is_ts_arrow ts -> print_arrow_ty fmt tys
  | Tyapp (ts, tyl) when is_ts_tuple ts ->
      pp fmt "%a" (list ~sep:star print_ty) tyl
  | Tyapp (ts, [ ty ]) -> pp fmt "%a %a" print_ty ty print_ts_name ts
  | Tyapp (ts, tyl) ->
      pp fmt "(%a) %a" (list ~sep:comma print_ty) tyl print_ts_name ts

let print_ts fmt ts =
  pp fmt "@[%a %a%a@]"
    (list ~sep:comma ~first:lparens ~last:rparens print_tv)
    ts.ts_args Ident.pp_simpl (ts_ident ts)
    (fun fmt alias ->
      match alias with None -> () | Some ty -> pp fmt " [=%a]" print_ty ty)
    ts.ts_alias

let print_exn_type f = function
  | Exn_tuple tyl -> list ~sep:star print_ty f tyl
  | Exn_record args ->
      let print_arg f (id, ty) = pp f "%a:%a" Ident.pp_simpl id print_ty ty in
      list ~sep:semi ~first:rbrace ~last:lbrace print_arg f args

let print_xs f x = pp f "%a" Ident.pp_simpl x.xs_ident
