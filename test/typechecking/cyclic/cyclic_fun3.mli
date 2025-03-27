(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test : forall f y. f y = f *)

(* {gospel_expected|
[1] Internal error: no filename location for the following error
    Error: This expression has type 'a
           but an expression was expected of type 'b -> 'a
           The type variable 'a occurs inside 'b -> 'a.
    
|gospel_expected} *)
