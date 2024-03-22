type t = A | B
(*@ with x invariant 1 >= 0 *)

(* {gospel_expected|
   [125] File "invariant2.mli", line 1, characters 0-45:
         1 | type t = A | B
         2 | (*@ with x invariant 1 >= 0 *)
         Error: Invariant on public type t.
   |gospel_expected} *)
