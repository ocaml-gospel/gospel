(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type a t *)
(* {gospel_expected|
[1] File "invalid_type_params2.mli", line 11, characters 11-12:
    11 | (*@ type a t *)
                    ^
    Error: Syntax error
    
|gospel_expected} *)
