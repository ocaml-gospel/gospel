type t = A of int * int

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | A (1i, 1i) -> true
*)

(* EXPECTED
   [125] File "etuple.mli", line 6, characters 6-45:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A (0i, 0i).
*)
