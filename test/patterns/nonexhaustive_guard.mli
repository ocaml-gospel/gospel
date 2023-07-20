val f : int -> int
(*@ r = f x
  requires match x with
           | 1i -> true
           | _ when 1 = 2 -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_guard.mli", line 3, characters 11-82:
         3 | ...........match x with
         4 |            | 1i -> true
         5 |            | _ when 1 = 2 -> false
         Error: This pattern-matching may not be exhaustive because of the guard.
                Here is an example of a case that may not be matched:
                  0i.
   |gospel_expected} *)
