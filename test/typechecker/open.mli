(*@ open Int_not_bool1 *)

(* {gospel_expected|
   [125] File "./int_not_bool1.mli", line 1, characters 48-49:
         1 | (*@ function rec f (x: bool) (y: int): bool = f y x *)
                                                             ^
         Error: This term has type int but a term was expected of type bool.
   |gospel_expected} *)
