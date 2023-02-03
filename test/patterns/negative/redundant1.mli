val f : int -> int
(*@ y = f x
    ensures match x with
    | _ -> true
    | 1i -> false *)
(* {gospel_expected|
   [0] File "redundant1.mli", line 3, characters 12-58:
       3 | ............match x with
       4 |     | _ -> true
       5 |     | 1i -> false...
       Warning: The pattern-matching is redundant.
                Here is a case that is unused:
                  1i.
       OK
   |gospel_expected} *)
