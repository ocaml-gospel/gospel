val f : 'a list -> int
(*@ r = f l
    requires match l with
      | [] -> true
      | x::y::_ -> false
*)

(* EXPECTED
   [125] File "list1.mli", line 3, characters 13-69:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  infix :: (_, []).
*)
