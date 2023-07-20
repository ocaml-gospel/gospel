type t = { a : int }
(*@ invariant a >= 0 *)

(* {gospel_expected|
   [125] File "invariant1.mli", line 1, characters 0-44:
         1 | type t = { a : int }
         2 | (*@ invariant a >= 0 *)
         Error: Invariant on public type `t'.
   |gospel_expected} *)
