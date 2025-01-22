(*@ function f (x : integer) : integer *)
(*@ ensures old x = x *)
(* {gospel_expected|
   [125] File "old_in_fun.mli", line 2, characters 12-17:
         2 | (*@ ensures old x = x *)
                         ^^^^^
         Error: old operator is not allowed in specifications for pure functions.
   |gospel_expected} *)
