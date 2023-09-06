val f : int -> int
(*@ y = f x
    pure
    checks x > 0 *)

(* {gospel_expected|
   [125] File "pure_no_exceptions2.mli", line 2, characters 8-9:
         2 | (*@ y = f x
                     ^
         Error: Type checking error: a pure function cannot raise exceptions.
   |gospel_expected} *)
