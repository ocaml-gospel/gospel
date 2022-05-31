val f : int -> int * int
(*@ pure *)

val f : int -> int * int * int
(*@ pure *)

(* EXPECTED
   [0] OK
*)
