(*@ type t = { a: bool; b: integer } *)
(*@ type r = { a: bool; c: integer } *)

val f : int -> bool
(*@ ensures let x = { a = true; b = 42 } in true
 *)
(* {gospel_expected|
   [125] File "record_with_partially_same_fields.mli", line 5, characters 20-40:
         5 | (*@ ensures let x = { a = true; b = 42 } in true
                                 ^^^^^^^^^^^^^^^^^^^^
         Error: The record field b does not exist.
   |gospel_expected} *)
