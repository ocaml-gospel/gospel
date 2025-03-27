(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : 'a) : 'a = (n : 'b) *)

(* {gospel_expected|
[1] File "invalid_cast_tvar1.mli", line 11, characters 31-39:
    11 | (*@ function f (n : 'a) : 'a = (n : 'b) *)
                                        ^^^^^^^^
    Error: Mismatch between type 'a and type 'b
    
|gospel_expected} *)
