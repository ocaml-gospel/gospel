(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : (0, 0, 0) = (0, (0, 0)) *)

(* {gospel_expected|
[1] File "wrong_arity2.mli", line 11, characters 29-40:
    11 | (*@ axiom test : (0, 0, 0) = (0, (0, 0)) *)
                                      ^^^^^^^^^^^
    Error: Mismatch between type integer * integer * integer
           and type integer * (integer * integer)
    
|gospel_expected} *)
