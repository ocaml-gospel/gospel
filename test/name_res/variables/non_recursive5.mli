(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : let x = x in True *)

(* {gospel_expected|
[1] File "non_recursive5.mli", line 11, characters 25-26:
    11 | (*@ axiom test : let x = x in True *)
                                  ^
    Error: Unbound value x
    
|gospel_expected} *)
