(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (y : integer * integer)
                (x : integer * integer * integer) = x = y *)

(* {gospel_expected|
[1] File "wrong_arity4.mli", line 12, characters 56-57:
    12 |                 (x : integer * integer * integer) = x = y *)
                                                                 ^
    Error: Mismatch between type integer * integer
           and type integer * integer * integer
    
|gospel_expected} *)
