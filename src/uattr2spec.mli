(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

val signature :
  filename:string -> Ppxlib.Parsetree.signature -> Uast.s_signature
(** Parses the specifications contained in the attributes of the input parsetree
    and returns an annotated parsetree with Gospel specifications. *)

val structure :
  filename:string -> Ppxlib.Parsetree.structure -> Uast.s_structure
(** Parses the specifications contained in the attributes of the input OCaml
    structure and returns an annotated OCaml AST with Gospel specifications. *)
