val f : int -> int
(*@ y = f x
    ensures match x with
    | _ | _ when x = 1 -> true
    | _ -> false *)

(* This is not ambiguous as far as OCaml is concerned, maybe that error should
   be removed *)

(* {gospel_expected|
   [125] File "ambiguous.mli", line 4, characters 6-11:
         4 |     | _ | _ when x = 1 -> true
                   ^^^^^
         Error: Or-patterns are prohibited under guards to avoid ambiguities
                (see OCaml compiler warning 57).
   |gospel_expected} *)
