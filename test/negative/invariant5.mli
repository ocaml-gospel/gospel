type 'a t = private { a : 'a }
(*@ invariant self.a = self.a *)
