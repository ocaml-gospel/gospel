type t = { f : int }

val f : int -> int
(*@ pure *)

val g : int -> int
(*@ y = g x
    requires f x > 0 *)

(* EXPECTED
   [0] OK
*)
