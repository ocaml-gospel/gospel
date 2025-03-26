(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('b, 'a, 'a) t *)

(* {gospel_expected|
[1] File "duplicate_type_vars2.mli", line 11, characters 18-20:
    11 | (*@ type ('b, 'a, 'a) t *)
                           ^^
    Error: The type parameter a occurs several times
    
|gospel_expected} *)
