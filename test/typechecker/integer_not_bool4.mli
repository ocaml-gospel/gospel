(*@ function rec f (x:bool): bool = f 2 *)

(* ERROR: cannot match integer with bool *)

(* {gospel_expected|
   [125] File "integer_not_bool4.mli", line 1, characters 38-39:
         1 | (*@ function rec f (x:bool): bool = f 2 *)
                                                   ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
