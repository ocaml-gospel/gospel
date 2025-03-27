(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom test :
      forall x : 'a.
      forall y : 'b.
      forall z. x = z = y *)

(* {gospel_expected|
[1] File "neq_vars7.mli", line 14, characters 24-25:
    14 |       forall z. x = z = y *)
                                 ^
    Error: Mismatch between type 'b and type 'a
    
|gospel_expected} *)
