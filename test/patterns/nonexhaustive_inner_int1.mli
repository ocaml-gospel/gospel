type t = A of int * int

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | A (1i, 1i) -> true
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_int1.mli", line 6, characters 6-45:
         6 | ......match a with
         7 |       | A (1i, 1i) -> true
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A (0i, 0i).
   |gospel_expected} *)
