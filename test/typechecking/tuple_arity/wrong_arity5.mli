(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (x : integer * integer * integer)
                (y : integer * integer) = x = y *)

(* {gospel_expected|
[1] File "wrong_arity5.mli", line 12, characters 46-47:
    12 |                 (y : integer * integer) = x = y *)
                                                       ^
    Error: Mismatch between type integer * integer
           and type integer * integer * integer
    
|gospel_expected} *)
