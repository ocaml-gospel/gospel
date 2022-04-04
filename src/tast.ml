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
open Tterm
open Symbols
module Ident = Identifier.Ident

type lb_arg =
  | Lunit  (** () *)
  | Lnone of vsymbol  (** x *)
  | Loptional of vsymbol  (** ?x *)
  | Lnamed of vsymbol  (** ~x *)
  | Lghost of vsymbol  (** \[x: t\] *)
[@@deriving show]

type val_spec = {
  sp_args : lb_arg list;  (** Arguments *)
  sp_ret : lb_arg list;
      (** Return values. This is a list because of tuple destruction. *)
  sp_pre : term list;  (** Preconditions *)
  sp_checks : term list;  (** Checks preconditions *)
  sp_post : term list;  (** Postconditions *)
  sp_xpost : (xsymbol * (pattern * term) list) list;
      (** Exceptional postconditions. *)
  sp_wr : term list;  (** Writes *)
  sp_cs : term list;  (** Consumes *)
  sp_diverge : bool;  (** Diverges *)
  sp_pure : bool;  (** Pure *)
  sp_equiv : string list;  (** Equivalent *)
  sp_text : string;
      (** String containing the original specificaion as written by the user *)
  sp_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show]

type type_spec = {
  ty_ephemeral : bool;  (** Ephemeral *)
  ty_fields : (lsymbol * bool) list;  (** Models (field symbol * mutable) *)
  ty_invariants : term list;  (** Invariants *)
  ty_text : string;
      (** String containing the original specificaion as written by the user *)
  ty_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show]

type axiom = {
  ax_name : Ident.t;  (** Name *)
  ax_term : term;  (** Definition *)
  ax_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Location *)
  ax_text : string;
      (** String containing the original specificaion as written by the user *)
}
[@@deriving show]

type fun_spec = {
  fun_req : term list;  (** Preconditions *)
  fun_ens : term list;  (** Postconditions *)
  fun_variant : term list;  (** Variant *)
  fun_coer : bool;  (** Coercion *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Specification location *)
}
[@@deriving show]

type function_ = {
  fun_ls : lsymbol;  (** Function symbol *)
  fun_rec : bool;  (** Recursive *)
  fun_params : vsymbol list;  (** Arguments *)
  fun_def : term option;  (** Definition *)
  fun_spec : fun_spec option;  (** Specification *)
  fun_text : string;
      (** String containing the original specificaion as written by the user *)
  fun_loc : Location.t; [@printer fun fmt _ -> fprintf fmt "<Location.t>"]
      (** Location *)
}
[@@deriving show]

type floating =
  | Axiom of axiom
  | Function of function_
  | Value of value_description
  | Type of rec_flag * type_declaration list
  | Open of open_description
