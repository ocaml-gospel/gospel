(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type t = 'a sequence *)

(* {gospel_expected|
[1] File "unbound_var1.mli", line 11, characters 13-15:
    11 | (*@ type t = 'a sequence *)
                      ^^
    Error: The type variable 'a is unbound in this type declaration
    
|gospel_expected} *)
