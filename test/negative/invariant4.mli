type 'a t = private { a : 'a }
(*@ with self
    invariant self.a = 42 *)
