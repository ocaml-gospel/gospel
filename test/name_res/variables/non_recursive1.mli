(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f : integer = f *)

(* {gospel_expected|
[1] File "non_recursive1.mli", line 11, characters 27-28:
    11 | (*@ function f : integer = f *)
                                    ^
    Error: Unbound value f
    
|gospel_expected} *)
