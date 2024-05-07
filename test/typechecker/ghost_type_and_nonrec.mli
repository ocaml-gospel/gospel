(*@ type nonrec a = A and b = B of a *)
(* {gospel_expected|
   [125] File "ghost_type_and_nonrec.mli", line 1, characters 35-36:
         1 | (*@ type nonrec a = A and b = B of a *)
                                                ^
         Error: Symbol a not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
