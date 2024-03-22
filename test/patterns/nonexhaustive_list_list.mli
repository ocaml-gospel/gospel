val f : 'a list list -> int
(*@ r = f l
    requires match l with
      | [] -> true
      | []::_ -> false
      | (x::y::_)::_ -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_list_list.mli", line 3, characters 13-97:
         3 | .............match l with
         4 |       | [] -> true
         5 |       | []::_ -> false
         6 |       | (x::y::_)::_ -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  infix :: (infix :: (_, []), []).
   |gospel_expected} *)
