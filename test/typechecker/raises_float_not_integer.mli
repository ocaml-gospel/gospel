(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

exception E of float list

val f : 'a -> 'a
(*@ x = f y
    raises E l -> match l with
                  | [] -> false
                  | y :: ys -> y = 2
*)

(* {gospel_expected|
   [125] File "raises_float_not_integer.mli", line 17, characters 31-32:
         17 |                   | y :: ys -> y = 2
                                             ^
         Error: This term has type float but a term was expected of type integer.
   |gospel_expected} *)
