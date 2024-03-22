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
open Symbols
module Ident = Identifier.Ident

type pattern = {
  p_node : pattern_node;
  p_ty : ty;
  p_loc : Location.t; [@printer Utils.Fmt.pp_loc]
}
[@@deriving show]

and pattern_node =
  | Pwild  (** _ *)
  | Pvar of vsymbol  (** x *)
  | Papp of lsymbol * pattern list  (** Constructor *)
  | Por of pattern * pattern  (** p1 | p2 *)
  | Pas of pattern * vsymbol  (** p as vs *)
  | Pinterval of char * char
      [@printer fun fmt (c1, c2) -> fprintf fmt "%C..%C" c1 c2]
  | Pconst of constant
      [@printer
        fun fmt cc ->
          let t, v =
            match cc with
            | Pconst_integer (s, _) -> ("integer", s)
            | Pconst_char c -> ("char", Format.sprintf "%C" c)
            | Pconst_string (s, _, _) -> ("string", Format.sprintf "%S" s)
            | Pconst_float (s, _) -> ("float", s)
          in
          fprintf fmt "<const_%s: %s >" t v]
[@@deriving show]

type binop = Tand | Tand_asym | Tor | Tor_asym | Timplies | Tiff
[@@deriving show]

type quant = Tforall | Texists [@@deriving show]

type term = {
  t_node : term_node;
  t_ty : ty option;
  t_attrs : string list;
  t_loc : Location.t; [@printer Utils.Fmt.pp_loc]
}
[@@deriving show]

and term_node =
  | Tvar of vsymbol
  | Tconst of constant [@printer fun fmt _ -> fprintf fmt "<constant>"]
  | Tapp of lsymbol * term list
  | Tfield of term * lsymbol
  | Tif of term * term * term
  | Tlet of vsymbol * term * term
  | Tcase of term * (pattern * term option * term) list
  | Tquant of quant * vsymbol list * term
  | Tlambda of pattern list * term
  | Tbinop of binop * term * term
  | Tnot of term
  | Told of term
  | Ttrue
  | Tfalse
[@@deriving show]
