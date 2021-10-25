(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Tterm
open Symbols

val print_vs : Format.formatter -> vsymbol -> unit
val print_ls_decl : Format.formatter -> lsymbol -> unit
val print_ls_nm : Format.formatter -> lsymbol -> unit
val print_pattern : Format.formatter -> pattern -> unit
val print_binop : Format.formatter -> binop -> unit
val print_quantifier : Format.formatter -> quant -> unit
val print_term : Format.formatter -> term -> unit
