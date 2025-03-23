(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : True) *)
(* {gospel_expected|
[1] File "unmatched_paren2.mli", line 11, characters 21-22:
    11 | (*@ axiom test : True) *)
                              ^
    Error: Syntax error
    
|gospel_expected} *)
