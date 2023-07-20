val f : 'a option list -> bool
(*@ b = f os
    ensures List._exists (fun (None | Some _) -> false) os
*)

(* {gospel_expected|
   [125] File "fun_opt.mli", line 3, characters 30-31:
         3 |     ensures List._exists (fun (None | Some _) -> false) os
                                           ^
         Error: Syntax error.
   |gospel_expected} *)
