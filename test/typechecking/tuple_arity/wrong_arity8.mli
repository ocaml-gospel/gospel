(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ predicate f (x : (integer * integer) * integer)
                (y : integer * (integer * integer)) = x = y *)

(* {gospel_expected|
[1] File "wrong_arity8.mli", line 12, characters 58-59:
    12 |                 (y : integer * (integer * integer)) = x = y *)
                                                                   ^
    Error: Mismatch between type integer * (integer * integer)
           and type (integer * integer) * integer
           Type integer is incompatible with type integer * integer
    
|gospel_expected} *)
