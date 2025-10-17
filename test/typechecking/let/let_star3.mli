(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : forall x. let* y, z = x in True *)

(* {gospel_expected|
[1] File "let_star3.mli", line 11, characters 44-48:
    11 | (*@ axiom test : forall x. let* y, z = x in True *)
                                                     ^^^^
    Error: Mismatch between a type 'd -> prop
           and type 'c -> 'b -> 'a
           Mismatch between arity 1 and arity 2
    
|gospel_expected} *)
