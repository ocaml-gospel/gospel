type 'a t = P of 'a * int

val f : int t -> int
(*@ r = f a
    ensures
      match a with
      | P(x, 0i) -> true
      | P(1i, x) -> false
*)

(* EXPECTED
   [125] File "pair.mli", line 6, characters 6-69:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  P (0i, 1i).
*)
