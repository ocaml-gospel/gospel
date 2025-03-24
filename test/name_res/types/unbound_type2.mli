(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type test = t sequence *)

(* {gospel_expected|
[1] File "unbound_type2.mli", line 11, characters 16-17:
    11 | (*@ type test = t sequence *)
                         ^
    Error: Unbound type constructor t
    
|gospel_expected} *)
