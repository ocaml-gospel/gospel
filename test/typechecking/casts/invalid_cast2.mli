(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : integer) : integer = ((n + n) : prop) *)

(* {gospel_expected|
[1] File "invalid_cast2.mli", line 11, characters 41-57:
    11 | (*@ function f (n : integer) : integer = ((n + n) : prop) *)
                                                  ^^^^^^^^^^^^^^^^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
