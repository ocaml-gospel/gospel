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
open Identifier
open Uast

let mk_loc s e = {
  Location.loc_start = s;
  Location.loc_end = e;
  Location.loc_ghost = false;
}

let mk_pid pid s e = Preid.create pid ~attrs:[] ~loc:(mk_loc s e )
let mk_term d s e = { term_desc = d; term_loc = mk_loc s e }
let mk_pat  d s e = { pat_desc  = d; pat_loc  = mk_loc s e }

let get_op   s e = Qpreid (mk_pid (mixfix "[_]") s e)
let set_op   s e = Qpreid (mk_pid (mixfix "[<-]") s e)
let sub_op   s e = Qpreid (mk_pid (mixfix "[_.._]") s e)
let above_op s e = Qpreid (mk_pid (mixfix "[_..]") s e)
let below_op s e = Qpreid (mk_pid (mixfix "[.._]") s e)

let id_anonymous loc = Preid.create "_" ~attrs:[] ~loc
let array_get s e =
  Qdot (Qpreid (mk_pid "Array" s e), mk_pid (mixfix "[_]") s e)

let empty_fspec = {
  fun_req     = [];
  fun_ens     = [];
  fun_variant = [];
  fun_coer    = false;
}

let empty_tspec = {
  ty_ephemeral = false;
  ty_field = [];
  ty_invariant = [];
}

let pid_of_label = function
  | Lunit -> invalid_arg "pid_of_label Lunit"
  | Lnone p | Lquestion p | Lnamed p | Lghost (p,_) -> p

let loc_of_qualid = function
  | Qpreid pid | Qdot (_,pid) -> pid.pid_loc

let qualid_preid = function
  | Qpreid p | Qdot (_, p) -> p
