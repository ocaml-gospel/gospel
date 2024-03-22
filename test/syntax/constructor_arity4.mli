type t = L of t * t | E

val f : t -> int
(*@ r = f a
    ensures
      match a with
      | L ((x,y)) -> x=y
*)

(* {gospel_expected|
   [125] File "constructor_arity4.mli", line 7, characters 8-17:
         7 |       | L ((x,y)) -> x=y
                     ^^^^^^^^^
         Error: The constructor L expects 2 argument(s)
                but is applied to 1 argument(s) here.
   |gospel_expected} *)
