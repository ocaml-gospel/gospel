type t = A | B of t

val f : t -> int
(*@ y = f x
    ensures match x with
    | A | B _ -> false
    | B (B A) -> false
    | _ -> true *)

(* {gospel_expected|
   [125] File "redundant3.mli", line 5, characters 12-86:
         5 | ............match x with
         6 |     | A | B _ -> false
         7 |     | B (B A) -> false
         8 |     | _ -> true...
         Error: The pattern-matching is redundant.
                Here is a case that is unused:
                  B B A.
   |gospel_expected} *)
