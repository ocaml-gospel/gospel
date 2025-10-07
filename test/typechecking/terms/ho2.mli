(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom fail : (fun x -> x) = 0 *)

(* {gospel_expected|
[1] File "ho2.mli", line 11, characters 32-33:
    11 | (*@ axiom fail : (fun x -> x) = 0 *)
                                         ^
    Error: Mismatch between type 'a -> 'a and type integer
    
|gospel_expected} *)
