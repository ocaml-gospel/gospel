(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : (let x = 0 in x) + x *)

(* {gospel_expected|
[1] File "escape_scope1.mli", line 11, characters 36-37:
    11 | (*@ axiom test : (let x = 0 in x) + x *)
                                             ^
    Error: Unbound value x
    
|gospel_expected} *)
