(*@ function rec f (x: bool) (y: int): bool = f y x *)

(* ERROR: cannot match int with bool *)

(* EXPECTED
   [125] File "t9.mli", line 1, characters 45-46:
         Error: This term has type `int' but a term was expected of type `bool'.
*)
