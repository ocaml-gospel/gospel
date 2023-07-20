val f : int -> int
(*@ y = f x
    pure
    diverges *)

(* {gospel_expected|
   [125] File "pure_no_diverges.mli", line 2, characters 8-9:
         2 | (*@ y = f x
                     ^
         Error: Type checking error: a pure function cannot diverge.
   |gospel_expected} *)
