val f : int -> int
(*@ y = f x
    ensures match x with
    | _ | _ when x = 1 -> true
    | _ -> false *)

(* This is not ambiguous as far as OCaml is concerned, maybe that error should
   be removed *)

(* {gospel_expected|
   [125] File "ambiguous.mli", line 3, characters 12-72:
         3 | ............match x with
         4 |     | _ | _ when x = 1 -> true
         5 |     | _ -> false...
         Error: Ambiguous or-pattern under guard.
   |gospel_expected} *)
