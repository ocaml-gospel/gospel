(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(*@ axiom fail :
      forall f. f + f = f (fun x -> x) *)

(* {gospel_expected|
[1] File "ho4.mli", line 12, characters 24-25:
    12 |       forall f. f + f = f (fun x -> x) *)
                                 ^
    Error: Mismatch between type integer and type 'b -> 'a
    
|gospel_expected} *)
