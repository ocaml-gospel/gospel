(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Types

type tsymbol = { ts_id : Preid.t; ts_ty : ty }
(** Typed variables *)

(** Typed variables *)
let mk_ts ts_id ts_ty = { ts_id; ts_ty }

(** Typed terms *)
type term_node =
  | Ttrue
  | Tfalse
  | Tvar of Preid.t
  | Tlet of Preid.t * term * term
  | Tconst of Ppxlib.constant
  | Tapply of term * term
  | Tquant of Uast.quant * tsymbol list * term
  | Tif of term * term * term

and term = { t_node : term_node; t_ty : ty; t_loc : Location.t }

let mk_term t_node t_ty t_loc = { t_node; t_ty; t_loc }

(* Typed Signatures *)

type axiom = {
  ax_name : Preid.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_loc : Location.t;  (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Specification location *)
}

type function_ = {
  fun_name : Preid.t;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : tsymbol list;  (** Arguments *)
  fun_ret : ty;
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec option;  (** Specification *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t;  (** Location *)
}

type signature = Sig_function of function_ | Sig_axiom of axiom

(* Helper functions *)

let mk_function f fun_params fun_def fun_ret fun_spec =
  {
    fun_name = f.Uast.fun_name;
    fun_rec = f.fun_rec;
    fun_params;
    fun_def;
    fun_ret;
    fun_spec;
    fun_text = f.fun_text;
    fun_loc = f.fun_loc;
  }

let mk_axiom ax_name ax_term ax_loc ax_text =
  { ax_name; ax_term; ax_loc; ax_text }
