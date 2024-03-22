type tc = char * int
(*@ function f (a : tc) : bool =
      match a with
      | '\000'..'b', 0i -> true
      | 'b'..'\255', x -> false
*)

(* {gospel_expected|
   [125] File "nonexhaustive_pair2.mli", line 3, characters 6-82:
         3 | ......match a with
         4 |       | '\000'..'b', 0i -> true
         5 |       | 'b'..'\255', x -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  '\000', 1i.
   |gospel_expected} *)
