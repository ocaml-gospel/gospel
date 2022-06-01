val f : int -> int
(*@ y = f x *)

val g : int -> int
(*@ y = g x
    requires f x > 0 *)

(* {gospel_expected|
   [125] File "not_pure.mli", line 6, characters 13-14:
         6 |     requires f x > 0 *)
                          ^
         Error: Symbol f not found.
   |gospel_expected} *)
