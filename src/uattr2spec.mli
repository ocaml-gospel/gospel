(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val signature : filename:string -> Ppxlib.signature -> Parse_uast.s_signature
(** Parses the specifications contained in the attributes of the input parsetree
    and returns an annotated parsetree with Gospel specifications. *)
