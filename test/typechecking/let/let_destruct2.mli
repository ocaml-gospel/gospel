(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom wrong : forall x : (integer * integer). let w, y, z = x in True *)

(* {gospel_expected|
[1] File "let_destruct2.mli", line 11, characters 64-65:
    11 | (*@ axiom wrong : forall x : (integer * integer). let w, y, z = x in True *)
                                                                         ^
    Error: Mismatch between type integer * integer and type 'a * 'b * 'c
    
|gospel_expected} *)
