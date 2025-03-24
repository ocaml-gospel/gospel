(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type 'b t = 'a sequence *)

(* {gospel_expected|
[1] File "unbound_var2.mli", line 11, characters 16-18:
    11 | (*@ type 'b t = 'a sequence *)
                         ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
