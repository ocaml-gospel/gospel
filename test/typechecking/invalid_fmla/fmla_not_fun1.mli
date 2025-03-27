(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : (+) 0 *)

(* {gospel_expected|
[1] File "fmla_not_fun1.mli", line 11, characters 15-20:
    11 | (*@ axiom ax : (+) 0 *)
                        ^^^^^
    Error: Mismatch between type prop and type integer -> integer
    
|gospel_expected} *)
