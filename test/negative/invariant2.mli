type t = A | B
(*@ invariant 1 >= 0 *)

(* EXPECTED
   [125] File "invariant2.mli", line 1, characters 0-48:
         Error: Invariant on public type `t'.
*)
