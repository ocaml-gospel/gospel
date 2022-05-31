type t = A | B of t

val f : t -> int
(*@ y = f x
    ensures match x with
    | A | B _ -> false
    | B (B A) -> false
    | _ -> true *)
(* EXPECTED
   [125] File "redundant3.mli", line 5, characters 12-86:
         Error: The pattern-matching is redundant.
                Here is a case that is unused:
                  B B A.
*)
