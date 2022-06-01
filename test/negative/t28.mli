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
    | y :: ys -> y = 2 *)

(* ERROR:
   Line 13
   y is of type float and 2 of type integer
   replace "2" by "2." in line 13 *)

(* {gospel_expected|
   [125] File "t28.mli", line 13, characters 17-18:
         13 |     | y :: ys -> y = 2 *)
                               ^
         Error: This term has type `float' but a term was expected of type `integer'.
   |gospel_expected} *)
