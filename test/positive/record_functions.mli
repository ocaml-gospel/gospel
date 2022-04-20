type t = int -> int
type u = (int -> int) ref
type v = private { x : int -> int }
(*@ invariant self.x 0i = 0 *)
