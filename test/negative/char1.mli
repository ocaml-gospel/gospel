val f : char -> unit
(*@ f c
    requires c = '\' *)

(* EXPECTED
   [125] File "char1.mli", line 3, characters 17-18:
         Error: Illegal character `''.
*)
