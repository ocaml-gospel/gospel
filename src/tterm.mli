open Ppxlib
open Ttypes
module Ident = Identifier.Ident

type vsymbol = { vs_name : Ident.t; vs_ty : ty }

module Svs : Set.S with type elt = vsymbol

type lsymbol = {
  ls_name : Ident.t;
  ls_args : ty list;
  ls_value : ty option;
  ls_constr : bool;
  (* true if it is a construct, false otherwise*)
  ls_field : bool; (* true if it is a record/model field *)
}

type pattern = {
  p_node : pattern_node;
  p_ty : ty;
  p_vars : Svs.t;
  p_loc : Location.t option;
}

and pattern_node =
  | Pwild  (** _ *)
  | Pvar of vsymbol  (** x *)
  | Papp of lsymbol * pattern list  (** Constructor *)
  | Por of pattern * pattern  (** p1 | p2 *)
  | Pas of pattern * vsymbol  (** p as vs *)

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
type quant = Tforall | Texists | Tlambda

type term = {
  t_node : term_node;
  t_ty : ty option;
  t_attrs : string list;
  t_loc : Location.t;
}

and term_node =
  | Tvar of vsymbol
  | Tconst of Parsetree.constant
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
