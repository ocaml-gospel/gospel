val first : 'a array -> 'a
(*@ x = first a
    requires Sequence.length (old a) >= 1 *)

(* {gospel_expected|
   [125] File "old_in_precond.mli", line 3, characters 29-36:
         3 |     requires Sequence.length (old a) >= 1 *)
                                          ^^^^^^^
         Error: old operator is not allowed in requires clauses.
   |gospel_expected} *)
