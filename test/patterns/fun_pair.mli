val f : (int * 'a) list -> int list
(*@ ys = f xs
    ensures ys = List.map (fun (x, _) -> x) xs
*)

(* {gospel_expected|
   [125] File "fun_pair.mli", line 3, characters 31-32:
         3 |     ensures ys = List.map (fun (x, _) -> x) xs
                                            ^
         Error: Syntax error.
   |gospel_expected} *)
