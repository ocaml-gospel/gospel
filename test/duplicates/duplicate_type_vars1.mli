(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('a, 'a) t *)

(* {gospel_expected|
[1] File "duplicate_type_vars1.mli", line 11, characters 14-16:
    11 | (*@ type ('a, 'a) t *)
                       ^^
    Error: The type parameter a occurs several times
    
|gospel_expected} *)
