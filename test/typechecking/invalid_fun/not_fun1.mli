(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : 0 0 *)

(* {gospel_expected|
[1] File "not_fun1.mli", line 11, characters 17-18:
    11 | (*@ axiom test : 0 0 *)
                          ^
    Error: Mismatch between type 'b -> 'a and type integer
    
|gospel_expected} *)
