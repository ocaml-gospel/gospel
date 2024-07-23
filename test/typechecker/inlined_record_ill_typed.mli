(*@ type t = A of { b : bool } *)

(*@ function f (i : integer) : t = A { b = i }
*)

(* {gospel_expected|
   [125] File "inlined_record_ill_typed.mli", line 3, characters 43-44:
         3 | (*@ function f (i : integer) : t = A { b = i }
                                                        ^
         Error: This term has type integer but a term was expected of type bool.
   |gospel_expected} *)
