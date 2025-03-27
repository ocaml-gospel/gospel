(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax : forall s. Sequence.cons s s = Sequence.empty *)
(* {gospel_expected|
[1] Internal error: no filename location for the following error
    Error: This expression has type 'a
           but an expression was expected of type 'a sequence
           The type variable 'a occurs inside 'a sequence.
    
|gospel_expected} *)
