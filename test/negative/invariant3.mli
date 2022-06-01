type t = int * int
(*@ invariant 1 > 0 *)

(* {gospel_expected|
   [125] File "invariant3.mli", line 1, characters 0-51:
         Error: Invariant on public type `t'.
   |gospel_expected} *)
