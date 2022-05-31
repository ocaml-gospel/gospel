val f : int -> int
(*@ y = f x
    pure
    diverges *)

(* EXPECTED
   [125] File "impure2.mli", line 2, characters 5-6:
         Error: Type checking error: a pure function cannot diverge.
*)
