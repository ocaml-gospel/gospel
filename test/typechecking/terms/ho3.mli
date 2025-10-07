(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom fail : ((fun x -> x + x) (fun x -> x)) = 0 *)

(* {gospel_expected|
[1] File "ho3.mli", line 11, characters 35-47:
    11 | (*@ axiom fail : ((fun x -> x + x) (fun x -> x)) = 0 *)
                                            ^^^^^^^^^^^^
    Error: Mismatch between type integer and type 'a -> 'a
    
|gospel_expected} *)
