(*@ function rec f (x: bool) (y: int): bool = f x y *)

(*@ function g (a: int): integer =
      if (f true 2) then 1 else 2
*)

(* {gospel_expected|
   [125] File "integer_not_int.mli", line 4, characters 17-18:
         4 |       if (f true 2) then 1 else 2
                              ^
         Error: This term has type integer but a term was expected of type int.
   |gospel_expected} *)
