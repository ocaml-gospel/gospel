(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom wrong : forall x. x = False -> let* y = x in x *)

(* {gospel_expected|
[1] File "let_star2.mli", line 11, characters 50-51:
    11 | (*@ axiom wrong : forall x. x = False -> let* y = x in x *)
                                                           ^
    Error: Mismatch between type prop and type 'a option
    
|gospel_expected} *)
