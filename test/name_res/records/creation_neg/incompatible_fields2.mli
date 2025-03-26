(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M1 : sig
  (*@ type t1 = { x : integer; y : integer } *)
end

module M2 : sig
  (*@ type t2 = { x : integer; y : integer } *)
end

(*@ function t : M1.t1 = { M1.x = 0; M2.y = 0 } *)

(* {gospel_expected|
[1] File "incompatible_fields2.mli", line 19, characters 25-47:
    19 | (*@ function t : M1.t1 = { M1.x = 0; M2.y = 0 } *)
                                  ^^^^^^^^^^^^^^^^^^^^^^
    Error: The identifier M2.y belongs to the type M2.t2 but is mixed here with fields of type t1
    
|gospel_expected} *)
