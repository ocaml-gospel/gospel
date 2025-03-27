(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : if 0 then True else false *)

(* {gospel_expected|
[1] File "if.mli", line 11, characters 18-19:
    11 | (*@ axiom ax : if 0 then True else false *)
                           ^
    Error: Mismatch between type bool and type integer
    
|gospel_expected} *)
