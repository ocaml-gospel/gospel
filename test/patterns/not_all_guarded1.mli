type t = A

val f : t -> int
(*@ r = f x
  requires match x with
           | A when 1 = 1 -> true
           | _ when true  -> false
*)

(* {gospel_expected|
   [125] File "not_all_guarded1.mli", line 5, characters 11-92:
         5 | ...........match x with
         6 |            | A when 1 = 1 -> true
         7 |            | _ when true  -> false
         Error: All clauses in this pattern-matching are guarded.
   |gospel_expected} *)
