type t = private A | B of t | C of { x : t }
(*@ with self
    invariant match self with
              | A -> true
              | B t' -> self = t'
              | C a -> a.x = self *)

type u = private { tag : int; next : u }
(*@ with self
    invariant self.tag = self.next.tag = self.next.next.tag *)

val f : u -> u
(*@ y = f x
    requires x.tag = 0 *)
