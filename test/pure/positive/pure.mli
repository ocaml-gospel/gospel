type t = { f : int }

val f : int -> int
(*@ pure *)

val g : int -> int
(*@ y = g x
    requires f x > 0 *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
