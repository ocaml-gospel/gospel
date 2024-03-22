(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom ax: forall x: float list.
    match x with
    | y :: ys -> y = 2
*)

(* {gospel_expected|
   [125] File "axiom_float_not_integer.mli", line 13, characters 17-18:
         13 |     | y :: ys -> y = 2
                               ^
         Error: This term has type float but a term was expected of type integer.
   |gospel_expected} *)
