val f : int -> int
(*@ y = f x
    ensures match x with
    | _ | _ when x = 1 -> true
    | _ -> false *)
(* EXPECTED
   [125] File "ambiguous.mli", line 3, characters 12-72:
         Error: Ambiguous or-pattern under guard.
*)
