type 'a t = 'a * int

val f : int t -> int
(*@ r = f a
    ensures
      match a with
      | x, 0i -> true
      | 1i, x -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_int.mli", line 6, characters 6-63:
         6 | ......match a with
         7 |       | x, 0i -> true
         8 |       | 1i, x -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  0i, 1i.
   |gospel_expected} *)
