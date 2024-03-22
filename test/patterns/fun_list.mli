val f : 'a list -> 'a
(*@ x = f xs
    ensures x = (fun (_ :: y :: _ | y :: []) -> y) xs
*)

(* {gospel_expected|
   [125] File "fun_list.mli", line 3, characters 21-44:
         3 |     ensures x = (fun (_ :: y :: _ | y :: []) -> y) xs
                                  ^^^^^^^^^^^^^^^^^^^^^^^
         Error: This pattern-matching is not exhaustive.
                Here is an example of a case that is not matched:
                  [].
   |gospel_expected} *)
