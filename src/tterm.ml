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

module Vs = struct
  type t = vsymbol

  let equal x y = Ident.equal x.vs_name y.vs_name
  let compare x y = Ident.compare x.vs_name y.vs_name
end

module Svs = Set.Make (Vs)

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

let ls_equal l1 l2 = Ident.equal l1.ls_name l2.ls_name

module LS = struct
  type t = lsymbol

  let compare l1 l2 = Ident.compare l1.ls_name l2.ls_name
  let equal = ls_equal
  let hash = (Hashtbl.hash : lsymbol -> int)
end

module Sls = Set.Make (LS)

module Mls = Map.Make (LS)
(** terms *)

type pattern = {
  p_node : pattern_node;
  p_ty : ty;
  p_vars : Svs.t; [@printer fun fmt _ -> fprintf fmt "<Svs.t>"]
  p_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "Location.t"]
}
[@@deriving show]

and pattern_node =
  | Pwild  (** _ *)
  | Pvar of vsymbol  (** x *)
  | Papp of lsymbol * pattern list  (** Constructor *)
  | Por of pattern * pattern  (** p1 | p2 *)
  | Pas of pattern * vsymbol  (** p as vs *)
[@@deriving show]

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
[@@deriving show]

type quant = Tforall | Texists | Tlambda [@@deriving show]

type term = {
  t_node : term_node;
  t_ty : ty option;
  t_attrs : string list;
  t_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "Location.t"]
}
[@@deriving show]

and term_node =
  | Tvar of vsymbol
  | Tconst of Parsetree.constant
      [@printer fun fmt _ -> fprintf fmt "Parsetree.constant"]
  | Tapp of lsymbol * term list
  | Tfield of term * lsymbol
  | Tif of term * term * term
  | Tlet of vsymbol * term * term
  | Tcase of term * (pattern * term) list
  | Tquant of quant * vsymbol list * term
  | Tbinop of binop * term * term
  | Tnot of term
  | Told of term
  | Ttrue
  | Tfalse
[@@deriving show]
