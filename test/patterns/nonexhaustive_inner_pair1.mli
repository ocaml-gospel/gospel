type t12 = P of char * int

(*@ function f (a : t12) : bool =
      match a with
      | P ('\000'..'b', 0i) -> true
      | P ('b'..'\255', x) -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_inner_pair1.mli", line 4, characters 6-90:
         4 | ......match a with
         5 |       | P ('\000'..'b', 0i) -> true
         6 |       | P ('b'..'\255', x) -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  P ('\000', 1i).
   |gospel_expected} *)
