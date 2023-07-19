val f : 'a list -> 'a
(*@ x = f xs
    ensures x = (fun (_ :: y :: _ | y :: []) -> y) xs
*)

(* {gospel_expected|
   [125] File "fun_list.mli", line 3, characters 21-22:
         3 |     ensures x = (fun (_ :: y :: _ | y :: []) -> y) xs
                                  ^
         Error: Syntax error.
   |gospel_expected} *)
