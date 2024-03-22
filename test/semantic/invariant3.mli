type t = int * int
(*@ with x invariant 1 > 0 *)

(* {gospel_expected|
   [125] File "invariant3.mli", line 1, characters 0-48:
         1 | type t = int * int
         2 | (*@ with x invariant 1 > 0 *)
         Error: Invariant on public type t.
   |gospel_expected} *)
