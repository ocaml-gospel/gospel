(*@ type t = A of { b : bool } *)

(*@ function f (i : integer) : t = A { b = i }
*)

(* {gospel_expected|
   [125] File "inlined_record_ill_typed.mli", line 3, characters 37-46:
         3 | (*@ function f (i : integer) : t = A { b = i }
                                                  ^^^^^^^^^
         Error: Unbound record field: b.
   |gospel_expected} *)
