type t = int -> int
type u = (int -> int) ref
type v = private { x : int -> int }
(*@ invariant x 0i = 0 *)
