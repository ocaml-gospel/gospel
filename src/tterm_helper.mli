(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module Ident = Identifier.Ident
open Ppxlib
open Tterm
open Ttypes
open Symbols

val t_free_vars : Tterm.term -> Svs.t
val t_free_vs_in_set : Svs.t -> Tterm.term -> unit
val t_prop : Tterm.term -> Tterm.term
val t_type : term -> ty
val t_ty_check : term -> ty option -> unit
val ls_arg_inst : lsymbol -> term list -> ty Mtv.t
val ls_app_inst : lsymbol -> term list -> ty option -> Location.t -> ty Mtv.t
val mk_pattern : pattern_node -> ty -> Location.t -> pattern
val p_wild : ty -> Location.t -> pattern
val p_var : vsymbol -> Location.t -> pattern
val p_app : lsymbol -> pattern list -> ty -> Location.t -> pattern
val p_or : pattern -> pattern -> Location.t -> pattern
val p_as : pattern -> vsymbol -> Location.t -> pattern
val p_interval : char -> char -> Location.t -> pattern
val p_const : Parsetree.constant -> Location.t -> pattern
val mk_term : term_node -> ty option -> Location.t -> term
val t_var : vsymbol -> Location.t -> term
val t_const : constant -> ty -> Location.t -> term
val t_app : lsymbol -> term list -> ty option -> Location.t -> term
val t_field : term -> lsymbol -> ty option -> Location.t -> term
val t_if : term -> term -> term -> Location.t -> term
val t_let : vsymbol -> term -> term -> Location.t -> term
val t_case : term -> (pattern * term option * term) list -> Location.t -> term
val t_lambda : pattern list -> term -> ty option -> Location.t -> term
val t_binop : binop -> term -> term -> Location.t -> term
val t_not : term -> Location.t -> term
val t_old : term -> Location.t -> term
val t_true : Location.t -> term
val t_false : Location.t -> term
val t_attr_set : string list -> term -> term
val t_bool_true : Location.t -> term
val t_bool_false : Location.t -> term
val t_equ : term -> term -> Location.t -> term
val t_neq : term -> term -> Location.t -> Location.t -> term
val f_binop : binop -> term -> term -> Location.t -> term
val f_not : term -> Location.t -> term
val t_quant : quant -> vsymbol list -> term -> ty option -> Location.t -> term
val f_forall : vsymbol list -> term -> ty option -> Location.t -> term
val f_exists : vsymbol list -> term -> ty option -> Location.t -> term
val f_and : term -> term -> Location.t -> term
val f_and_asym : term -> term -> Location.t -> term
val f_or : term -> term -> Location.t -> term
val f_or_asym : term -> term -> Location.t -> term
val f_implies : term -> term -> Location.t -> term
val f_iff : term -> term -> Location.t -> term
