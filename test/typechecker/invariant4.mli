type 'a t = private { a : 'a }
(*@ with self
    invariant self.a = 42
*)

(* {gospel_expected|
   [125] File "invariant4.mli", line 3, characters 14-20:
         3 |     invariant self.a = 42
                           ^^^^^^
         Error: This term has type 'a but a term was expected of type integer.
   |gospel_expected} *)
