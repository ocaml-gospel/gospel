type t = { a : int }
(*@ with t invariant t.a >= 0 *)

(* {gospel_expected|
   [125] File "invariant1.mli", line 1, characters 0-53:
         1 | type t = { a : int }
         2 | (*@ with t invariant t.a >= 0 *)
         Error: Invariant on public type t.
   |gospel_expected} *)
