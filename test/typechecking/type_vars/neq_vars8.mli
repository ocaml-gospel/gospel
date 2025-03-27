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
      forall x : 'a sequence.
      forall y : 'b.
      Sequence.hd x = y *)

(* {gospel_expected|
[1] File "neq_vars8.mli", line 14, characters 22-23:
    14 |       Sequence.hd x = y *)
                               ^
    Error: Mismatch between type 'b and type 'a
    
|gospel_expected} *)
