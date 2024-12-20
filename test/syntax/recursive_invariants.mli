type t = private A | B of t | C of { x : t }
(*@ with self
    invariant match self with
              | A -> true
              | B t' -> self = t'
              | C a -> a.x = self *)

type u = private { tag : int; next : u }
(*@ with self
    invariant self.tag.v = self.next.tag.v = self.next.next.tag.v *)

val f : u -> u
(*@ y = f x
    requires x.tag.v = 0 *)
