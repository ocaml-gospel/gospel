val first : 'a array -> 'a
(*@ x = first a
    requires Array.length (old a) >= 1
*)

(* {gospel_expected|
   [125] File "old_in_precond.mli", line 3, characters 26-33:
         3 |     requires Array.length (old a) >= 1
                                       ^^^^^^^
         Error: old operator is not allowed in requires clauses.
   |gospel_expected} *)
