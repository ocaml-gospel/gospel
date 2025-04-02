(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ open M *)

(* {gospel_expected|
[1] File "module_not_found1.mli", line 11, characters 9-10:
    11 | (*@ open M *)
                  ^
    Error: Unbound module M
    
|gospel_expected} *)
