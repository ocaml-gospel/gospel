val f : int -> int
(*@ y = f x
    ensures match x with
    | _ -> true
    | 1i -> false *)
(* EXPECTED
   [125] File "redundant1.mli", line 3, characters 12-58:
         Error: The pattern-matching is redundant.
                Here is a case that is unused:
                  1i.
*)
