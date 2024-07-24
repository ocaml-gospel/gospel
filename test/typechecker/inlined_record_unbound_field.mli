(*@ type t = A of { b : bool } *)

(*@ function f (b : bool) : t = A { b; i = 42 } *)
(* {gospel_expected|
   [125] File "inlined_record_unbound_field.mli", line 3, characters 34-47:
         3 | (*@ function f (b : bool) : t = A { b; i = 42 } *)
                                               ^^^^^^^^^^^^^
         Error: Unbound record field: b.
   |gospel_expected} *)
