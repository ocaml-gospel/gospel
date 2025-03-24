(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('c, 'b) t = ('a, 'b) map *)

(* {gospel_expected|
[1] File "unbound_var4.mli", line 11, characters 23-25:
    11 | (*@ type ('c, 'b) t = ('a, 'b) map *)
                                ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
