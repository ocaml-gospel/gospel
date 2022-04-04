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
open Parsetree
module Preid = Identifier.Preid

(* Types *)

type qualid = Qpreid of Preid.t | Qdot of qualid * Preid.t

type pty =
  | PTtyvar of Preid.t
  | PTtyapp of qualid * pty list
  | PTtuple of pty list
  | PTarrow of labelled_arg * pty * pty

and labelled_arg =
  | Lunit
  | Lnone of Preid.t
  | Loptional of Preid.t
  | Lnamed of Preid.t
  | Lghost of Preid.t * pty

(* Patterns *)

type pattern = { pat_desc : pat_desc; pat_loc : Location.t }

and pat_desc =
  | Pwild
  | Pvar of Preid.t
  | Papp of qualid * pattern list
  | Prec of (qualid * pattern) list
  | Ptuple of pattern list
  | Pas of pattern * Preid.t
  | Por of pattern * pattern
  | Pcast of pattern * pty

(* Logical terms and formulas *)

type binder = Preid.t * pty option
type param = Location.t * Preid.t * pty
type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
type quant = Tforall | Texists | Tlambda

type term = { term_desc : term_desc; term_loc : Location.t }

and term_desc =
  | Ttrue
  | Tfalse
  | Tconst of constant
  | Tpreid of qualid
  | Tidapp of qualid * term list
  | Tfield of term * qualid
  | Tapply of term * term
  | Tinfix of term * Preid.t * term
  | Tbinop of term * binop * term
  | Tnot of term
  | Tif of term * term * term
  | Tquant of quant * binder list * term
  | Tattr of string * term
  | Tlet of Preid.t * term * term
  | Tcase of term * (pattern * term) list
  | Tcast of term * pty
  | Ttuple of term list
  | Trecord of (qualid * term) list
  | Tupdate of term * (qualid * term) list
  | Tscope of qualid * term
  | Told of term

(* Specification *)

type xpost = Location.t * (qualid * (pattern * term) option) list

type spec_header = {
  sp_hd_nm : Preid.t;
  (* header name *)
  sp_hd_ret : labelled_arg list;
  (* Can only be LNone or LGhost *)
  sp_hd_args : labelled_arg list; (* header arguments' names *)
}

type val_spec = {
  sp_header : spec_header option;
  sp_pre : term list;
  sp_checks : term list;
  sp_post : term list;
  sp_xpost : xpost list;
  sp_writes : term list;
  sp_consumes : term list;
  sp_diverge : bool;
  sp_pure : bool;
  sp_equiv : string list;
  sp_text : string;
  sp_loc : Location.t;
}

type field = {
  f_loc : Location.t;
  f_preid : Preid.t;
  f_pty : pty;
  f_mutable : bool;
}

type type_spec = {
  ty_ephemeral : bool;
  ty_field : field list;
  ty_invariant : term list;
  ty_text : string;
  ty_loc : Location.t;
}

type fun_spec = {
  fun_req : term list;
  fun_ens : term list;
  fun_variant : term list;
  fun_coer : bool;
  fun_text : string;
  fun_loc : Location.t;
}

type function_ = {
  fun_name : Preid.t;
  fun_rec : bool;
  fun_type : pty option;
  fun_params : param list;
  fun_def : term option;
  fun_spec : fun_spec option;
  fun_loc : Location.t;
  fun_text : string;
}

type axiom = {
  ax_name : Preid.t;
  ax_term : term;
  ax_loc : Location.t;
  ax_text : string;
}

type floating =
  | Axiom of axiom
  | Function of function_
  | Value of value_description
  | Type of rec_flag * type_declaration list
  | Open of open_description
