(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('b, 'c) t = { x : 'a; y : 'b } *)

(* {gospel_expected|
[1] File "unbound_var9.mli", line 11, characters 28-30:
    11 | (*@ type ('b, 'c) t = { x : 'a; y : 'b } *)
                                     ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
