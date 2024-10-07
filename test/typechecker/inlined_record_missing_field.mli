(*@ type t = A of { a: bool; b : bool } *)

(*@ function f (b : bool) : t = A { b; } *)
(* {gospel_expected|
   [125] File "inlined_record_missing_field.mli", line 3, characters 32-40:
         3 | (*@ function f (b : bool) : t = A { b; } *)
                                             ^^^^^^^^
         Error: Some record fields are undefined: a.
   |gospel_expected} *)
