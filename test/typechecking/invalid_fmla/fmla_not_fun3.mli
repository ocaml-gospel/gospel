(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : (=) *)

(* {gospel_expected|
[1] File "fmla_not_fun3.mli", line 11, characters 15-18:
    11 | (*@ axiom ax : (=) *)
                        ^^^
    Error: Mismatch between type prop and type 'a -> 'a -> prop
    
|gospel_expected} *)
