val f : Stdlib.Float.Array.t -> int
(*@ l = f xs
    ensures l >= 0 *)

(* {gospel_expected|
   [125] File "t7.mli", line 1, characters 8-28:
         1 | val f : Stdlib.Float.Array.t -> int
                     ^^^^^^^^^^^^^^^^^^^^
         Error: Symbol Stdlib.Float.Array.t not found.
   |gospel_expected} *)
