type t = A | B of t

val f : t -> int
(*@ y = f x
    ensures match x with
    | A -> false
    | B (B _) -> false
    | B (B A) -> false
    | _ -> true *)
(* {gospel_expected|
   [125] File "redundant2.mli", line 5, characters 12-103:
         Error: The pattern-matching is redundant.
                Here is a case that is unused:
                  B B A.
   |gospel_expected} *)
