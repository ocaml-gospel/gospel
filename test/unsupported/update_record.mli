(*@ type r = { a : bool; b : integer } *)

type t
(*@ model r : r *)

val f : t -> t
(*@ y = f x
    ensures y.r = { x.r with b = 42 }
*)

(* {gospel_expected|
   [125] File "update_record.mli", line 8, characters 21-22:
         8 |     ensures y.r = { x.r with b = 42 }
                                  ^
         Error: Syntax error.
   |gospel_expected} *)
