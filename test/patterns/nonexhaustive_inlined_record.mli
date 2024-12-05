(*@ type t = A of { a : bool } *)

(*@ predicate p (t : t) =
      match t with
        | A { a = true } -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inlined_record.mli", line 4, characters 6-52:
         4 | ......match t with
         5 |         | A { a = true } -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A false.
   |gospel_expected} *)
