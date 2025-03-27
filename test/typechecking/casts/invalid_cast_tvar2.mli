(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (n : 'a) (f : 'a -> 'a) : 'a = (f : 'a) (n : 'a) *)

(* {gospel_expected|
[1] File "invalid_cast_tvar2.mli", line 11, characters 46-54:
    11 | (*@ function f (n : 'a) (f : 'a -> 'a) : 'a = (f : 'a) (n : 'a) *)
                                                       ^^^^^^^^
    Error: Mismatch between type 'a -> 'a and type 'a
    
|gospel_expected} *)
