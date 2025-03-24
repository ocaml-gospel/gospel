(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type test = integer * t *)

(* {gospel_expected|
[1] File "unbound_type6.mli", line 11, characters 26-27:
    11 | (*@ type test = integer * t *)
                                   ^
    Error: Unbound type constructor t
    
|gospel_expected} *)
