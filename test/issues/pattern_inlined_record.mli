(*@ type m = A of { a : bool } *)

(*@ function f (a : m) : bool =
      match a with
       | A { a } -> a
*)
(* {gospel_expected|
   [125] File "pattern_inlined_record.mli", line 5, characters 11-16:
         5 |        | A { a } -> a
                        ^^^^^
         Error: Unbound record field: a.
   |gospel_expected} *)
