val f : 'a list -> int
(*@ r = f l
    requires match l with
      | [] -> true
      | x::y::_ -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_list.mli", line 3, characters 13-69:
         3 | .............match l with
         4 |       | [] -> true
         5 |       | x::y::_ -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  infix :: (_, []).
   |gospel_expected} *)
