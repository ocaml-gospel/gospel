(*@ open T9 *)

(* ERROR: should fail when loading T9 *)

(* {gospel_expected|
   [125] File "./t9.mli", line 1, characters 45-46:
         Error: This term has type `int' but a term was expected of type `bool'.
   |gospel_expected} *)
