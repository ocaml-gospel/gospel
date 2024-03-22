val f : bool -> int
(*@ r = f x
    ensures
      match x with
      | true
      | true
      | true -> r <> 1
      | true -> true
    ensures
      match true with
      | _ -> true
    ensures
      match true with
      | x -> true
*)

(* {gospel_expected|
   [125] File "nonexhaustive_bool.mli", line 4, characters 6-88:
         4 | ......match x with
         5 |       | true
         6 |       | true
         7 |       | true -> r <> 1
         8 |       | true -> true
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  false.
   |gospel_expected} *)
