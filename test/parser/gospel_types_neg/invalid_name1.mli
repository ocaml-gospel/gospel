(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ type T *)
(* {gospel_expected|
[1] File "invalid_name1.mli", line 11, characters 9-10:
    11 | (*@ type T *)
                  ^
    Error: Syntax error
    
|gospel_expected} *)
