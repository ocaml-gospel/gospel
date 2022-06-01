type 'a t = private { a : 'a }
(*@ invariant self.a = self.a *)

(* {gospel_expected|
   [125] File "invariant5.mli", line 2, characters 14-18:
         2 | (*@ invariant self.a = self.a *)
                           ^^^^
         Error: Symbol self not found.
   |gospel_expected} *)
