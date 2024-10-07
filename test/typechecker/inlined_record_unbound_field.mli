(*@ type t = A of { b : bool } *)

(*@ function f (b : bool) : t = A { b; i = 42 } *)
(* {gospel_expected|
   [125] File "inlined_record_unbound_field.mli", line 3, characters 39-40:
         3 | (*@ function f (b : bool) : t = A { b; i = 42 } *)
                                                    ^
         Error: The field i is not part of the record argument for the constructor t.A.
   |gospel_expected} *)
