type t7 = char * char

val f7 : t7 -> int
(*@ r = f7 a
    ensures
      match a with
      | '\000', x -> true
      | x, '\000' -> true
*)

(* {gospel_expected|
   [125] File "nonexhaustive_char.mli", line 6, characters 6-70:
         6 | ......match a with
         7 |       | '\000', x -> true
         8 |       | x, '\000' -> true
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  '\001', '\001'.
   |gospel_expected} *)
