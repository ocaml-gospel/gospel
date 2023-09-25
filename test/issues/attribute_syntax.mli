val f : int -> (*@ misplaced *) int

(* {gospel_expected|
   [125] File "attribute_syntax.mli", line 1, characters 15-18:
         1 | val f : int -> (*@ misplaced *) int
                            ^^^
         Error: Syntax error.
   |gospel_expected} *)
