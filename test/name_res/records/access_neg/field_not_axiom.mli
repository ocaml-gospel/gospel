(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : True *)

(*@ axiom test : forall r. r.ax *)

(* {gospel_expected|
[1] File "field_not_axiom.mli", line 13, characters 29-31:
    13 | (*@ axiom test : forall r. r.ax *)
                                      ^^
    Error: Unbound record label ax
    
|gospel_expected} *)
