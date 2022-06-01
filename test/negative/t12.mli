(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): float =
      if (f true a) then 1. else 2. *)

(*@ function int_of_integer (x:integer): int *)

(*@ function h (a:int) (b:bool) (c:'a): bool =
      if a = int_of_integer 2
      then f b (int_of_integer 3)
      else g (int_of_integer 4) = (int_of_integer 5)
*)

(* ERROR:
   Line 21
   type mysmatch float and int
   replace "int_of_integer 5" by "5." in line 21 *)

(* {gospel_expected|
   [125] File "t12.mli", line 21, characters 11-12:
         21 |       else g (int_of_integer 4) = (int_of_integer 5)
                         ^
         Error: This term has type `float' but a term was expected of type `int'.
   |gospel_expected} *)
