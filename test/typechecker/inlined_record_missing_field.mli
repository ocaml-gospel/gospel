(*@ type t = A of { a: bool; b : bool } *)

(*@ function f (b : bool) : t = A { b; } *)
(* {gospel_expected|
   [125] File "inlined_record_missing_field.mli", line 3, characters 32-33:
         3 | (*@ function f (b : bool) : t = A { b; } *)
                                             ^
         Error: The symbol A cannot be partially applied.
   |gospel_expected} *)
