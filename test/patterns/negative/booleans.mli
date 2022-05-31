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

(* EXPECTED
   [125] File "booleans.mli", line 4, characters 6-88:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  False.
*)
