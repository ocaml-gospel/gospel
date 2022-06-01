val f : char -> unit
(*@ f c
    requires c = '\' *)

(* {gospel_expected|
   [125] File "char1.mli", line 3, characters 17-18:
         Error: Illegal character `''.
   |gospel_expected} *)
