type t1 = private { a : int }
(*@ with x invariant x.a >= 0 *)

type t2 = private A | B
(*@ with x invariant 1 > 0 *)

type t3 = private int * int
(*@ with x invariant 1 > 0 *)
