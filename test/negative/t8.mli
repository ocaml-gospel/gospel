(*@ function rec f (x:bool): bool = f 2 *)

(* ERROR: cannot match integer with bool *)

(* EXPECTED
   [125] File "t8.mli", line 1, characters 35-36:
         Error: This term has type `integer' but a term was expected of type `bool'.
*)
