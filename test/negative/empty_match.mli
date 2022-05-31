val f : int -> int
(*@ y = f x
    requires match x with *)

(* EXPECTED
   [125] File "empty_match.mli", line 3, characters 26-26:
         Error: Syntax error.
*)
