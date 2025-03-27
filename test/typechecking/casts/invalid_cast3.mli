(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : integer) : integer = (((n : integer) + (n : integer)) : prop) *)

(* {gospel_expected|
[1] File "invalid_cast3.mli", line 11, characters 41-81:
    11 | (*@ function f (n : integer) : integer = (((n : integer) + (n : integer)) : prop) *)
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
