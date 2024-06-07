(*@ type m = A of { a : bool } *)

type t
(*@ model m : m *)

val f : t -> t
(*@ y = f x
    ensures y.m = match x.m with A { a } -> not a *)
(* {gospel_expected|
   [125] File "pattern_inlined_record.mli", line 8, characters 37-38:
         8 |     ensures y.m = match x.m with A { a } -> not a *)
                                                  ^
         Error: Symbol a not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
