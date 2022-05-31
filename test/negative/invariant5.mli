type 'a t = private { a : 'a }
(*@ invariant self.a = self.a *)

(* EXPECTED
   [125] File "invariant5.mli", line 2, characters 11-15:
         Error: Symbol self not found.
*)
