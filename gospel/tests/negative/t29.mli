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
                  | y :: ys -> y = 2 *)

(* ERROR:
   Line 16
   y is of type float and 2 of type integer
   replace "2" by "2." in line 18 *)
