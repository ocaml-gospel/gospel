(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'b t = { x : 'a } *)

(* {gospel_expected|
[1] File "unbound_var6.mli", line 11, characters 22-24:
    11 | (*@ type 'b t = { x : 'a } *)
                               ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
