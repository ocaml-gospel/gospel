(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : integer) : integer =
      (((n : prop) + (n : integer)) : integer) *)

(* {gospel_expected|
[1] File "invalid_cast4.mli", line 12, characters 8-18:
    12 |       (((n : prop) + (n : integer)) : integer) *)
                 ^^^^^^^^^^
    Error: Mismatch between type integer and type prop
    
|gospel_expected} *)
