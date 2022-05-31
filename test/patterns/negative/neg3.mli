type t7 = char * char

val f7 : t7 -> int
(*@ r = f7 a
    ensures
      match a with
      | '\000', x -> true
      | x, '\000' -> true
*)

(* EXPECTED
   [125] File "neg3.mli", line 6, characters 6-70:
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  '\001', '\001'.
*)
