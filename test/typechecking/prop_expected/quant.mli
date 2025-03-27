(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : forall x. 0 *)

(* {gospel_expected|
[1] File "quant.mli", line 11, characters 25-26:
    11 | (*@ axiom ax : forall x. 0 *)
                                  ^
    Error: Mismatch between type prop and type integer
    
|gospel_expected} *)
