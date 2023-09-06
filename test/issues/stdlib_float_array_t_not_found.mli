val f : Stdlib.Float.Array.t -> int
(*@ l = f xs
    ensures l >= 0 *)

(* {gospel_expected|
   [125] File "stdlib_float_array_t_not_found.mli", line 1, characters 8-28:
         1 | val f : Stdlib.Float.Array.t -> int
                     ^^^^^^^^^^^^^^^^^^^^
         Error: Symbol Stdlib.Float.Array.t not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
