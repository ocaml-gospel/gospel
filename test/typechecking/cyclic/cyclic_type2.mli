(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type ('a, 'b) map *)

(*@ function bind (m : ('a, 'b) map) (k : 'a) (v : 'b) : ('a, 'b) map *)

(*@ axiom test : forall m k. bind m k m = m *)
(* {gospel_expected|
[1] Internal error: no filename location for the following error
    Error: This expression has type 'b
           but an expression was expected of type ('a, 'b) map
           The type variable 'b occurs inside ('a, 'b) map.
    
|gospel_expected} *)
