(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module M : sig
  (*@ type t = { x : integer; y : integer } *)
end

(*@ type t = { x : integer; z : integer } *)

(*@ function f : t = { z = 0; M.y = 0 }*)

(* {gospel_expected|
[1] File "incompatible_fields5.mli", line 17, characters 21-39:
    17 | (*@ function f : t = { z = 0; M.y = 0 }*)
                              ^^^^^^^^^^^^^^^^^^
    Error: No record found with the provided labels
    
|gospel_expected} *)
