type t = L of t * t | E

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | L E E -> r = 1
*)

(* {gospel_expected|
   [125] File "constructor_arity3.mli", line 7, characters 12-13:
         7 |       | L E E -> r = 1
                         ^
         Error: Syntax error.
   |gospel_expected} *)
