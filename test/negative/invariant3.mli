type t = int * int
(*@ invariant 1 > 0 *)

(* {gospel_expected|
   [125] File "invariant3.mli", line 1, characters 0-41:
         1 | type t = int * int
         2 | (*@ invariant 1 > 0 *)
         Error: Invariant on public type `t'.
   |gospel_expected} *)
