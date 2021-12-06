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

val print_vs : vsymbol Fmt.t
val print_ls_decl : lsymbol Fmt.t
val print_ls_nm : lsymbol Fmt.t
val print_pattern : pattern Fmt.t
val print_binop : binop Fmt.t
val print_quantifier : quant Fmt.t
val print_term : term Fmt.t
