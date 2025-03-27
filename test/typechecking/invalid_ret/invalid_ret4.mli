(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x : integer) : integer *)

(*@ function g (x : integer) : 'a = f x *)
(* {gospel_expected|
[1] File "invalid_ret4.mli", line 13, characters 36-39:
    13 | (*@ function g (x : integer) : 'a = f x *)
                                             ^^^
    Error: Mismatch between type 'a and type integer
    
|gospel_expected} *)
