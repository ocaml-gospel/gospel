type 'a t = private { a : 'a }
(*@ with t invariant self.a = self.a *)

(* {gospel_expected|
   [125] File "invariant5.mli", line 2, characters 21-25:
         2 | (*@ with t invariant self.a = self.a *)
                                  ^^^^
         Error: Symbol self not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
