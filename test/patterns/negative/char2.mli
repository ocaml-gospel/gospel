type t = A of char
(*@ function f2 (x : t) : bool =
      match x with
      | A ('\000'..'\010') -> false
      | A ('\011'..'a')
      | A ('c'..'\255') -> false
*)

(* {gospel_expected|
   [125] File "char2.mli", line 3, characters 6-111:
         3 | ......match x with
         4 |       | A ('\000'..'\010') -> false
         5 |       | A ('\011'..'a')
         6 |       | A ('c'..'\255') -> false
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  A 'b'.
   |gospel_expected} *)
