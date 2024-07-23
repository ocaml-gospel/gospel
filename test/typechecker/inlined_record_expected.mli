(*@ type t = A of { b : bool } *)

(*@ function f (b : bool) : t = A b *)
(* The type-checker is expected to fail here *)
(* {gospel_expected|
   [125] File "inlined_record_expected.mli", line 3, characters 32-35:
         3 | (*@ function f (b : bool) : t = A b *)
                                             ^^^
         Error: This constructor expects an inlined record argument.
   |gospel_expected} *)
