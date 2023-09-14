type t = bool * int

(*@ function f (a : t) : bool =
      match a with
      | true, 0i -> true
      | false, x -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_pair1.mli", line 4, characters 6-69:
         4 | ......match a with
         5 |       | true, 0i -> true
         6 |       | false, x -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  true, 1i.
   |gospel_expected} *)
