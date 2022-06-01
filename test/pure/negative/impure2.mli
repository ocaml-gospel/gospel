val f : int -> int
(*@ y = f x
    pure
    diverges *)

(* {gospel_expected|
   [125] File "impure2.mli", line 2, characters 5-6:
         Error: Type checking error: a pure function cannot diverge.
   |gospel_expected} *)
