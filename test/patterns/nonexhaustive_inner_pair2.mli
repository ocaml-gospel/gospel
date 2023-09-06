type 'a t = P of 'a * int

val f : int t -> int
(*@ r = f a
    ensures
      match a with
      | P(x, 0i) -> true
      | P(1i, x) -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_pair2.mli", line 6, characters 6-69:
         6 | ......match a with
         7 |       | P(x, 0i) -> true
         8 |       | P(1i, x) -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  P (0i, 1i).
   |gospel_expected} *)
