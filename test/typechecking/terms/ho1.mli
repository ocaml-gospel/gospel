(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom fail : forall f : ((integer -> integer) -> integer). f ((/\) True) *)

(* {gospel_expected|
[1] File "ho1.mli", line 11, characters 65-76:
    11 | (*@ axiom fail : forall f : ((integer -> integer) -> integer). f ((/\) True) *)
                                                                          ^^^^^^^^^^^
    Error: Mismatch between type integer -> integer
           and type prop -> prop
           Type integer is incompatible with type prop
    
|gospel_expected} *)
