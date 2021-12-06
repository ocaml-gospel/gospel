
type t1 = private { a : int } (*@ invariant a >= 0 *)

type t2 = private A | B (*@ invariant 1 > 0 *)

type t3 = private (int * int) (*@ invariant 1 > 0 *)

