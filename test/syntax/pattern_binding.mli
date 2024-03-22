val f : int -> int
(*@ y = f x
    ensures match x with _ | a -> a = 1
*)

(* {gospel_expected|
   [125] File "pattern_binding.mli", line 3, characters 25-26:
         3 |     ensures match x with _ | a -> a = 1
                                      ^
         Error: The variable a does not appear in this pattern.
   |gospel_expected} *)
