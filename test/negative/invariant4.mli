type 'a t = private { a : 'a }
(*@ with self
    invariant self.a = 42 *)

(* EXPECTED
   [125] File "invariant4.mli", line 3, characters 14-20:
         Error: This term has type `'a' but a term was expected of type `integer'.
*)
