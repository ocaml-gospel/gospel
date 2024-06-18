(*@ type r = { a : bool; b : integer } *)

type t
(*@ model r : r *)

val f : t -> t
(*@ y = f x
    ensures y.r = { x.r with b = 42 }
*)

(* {gospel_expected|
   [125] File "update_record.mli", line 8, characters 20-23:
         8 |     ensures y.r = { x.r with b = 42 }
                                 ^^^
         Error: The record field a cannot be applied.
   |gospel_expected} *)
