(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function f (x:float): bool = x = 2 *)

(* {gospel_expected|
   [125] File "function_float_not_integer.mli", line 11, characters 33-34:
         11 | (*@ function f (x:float): bool = x = 2 *)
                                               ^
         Error: This term has type float but a term was expected of type integer.
   |gospel_expected} *)
