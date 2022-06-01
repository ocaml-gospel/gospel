val f : int -> int
(*@ y = f x
    pure
    raises Not_found -> true *)

(* {gospel_expected|
   [125] File "impure3.mli", line 2, characters 5-6:
         Error: Type checking error: a pure function cannot raise exceptions.
   |gospel_expected} *)
