val f : int -> int
(*@ y = f x
      requires 0 = match x with _ when match 1 with _ -> true -> 1 | _ -> 2 *)

(* missing parentheses around nested match *)
(* {gospel_expected|
   [125] File "guard2.mli", line 3, characters 76-76:
         Error: Syntax error.
   |gospel_expected} *)
