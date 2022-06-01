val f : int -> int * int
(*@ pure *)

val f : int -> int * int * int
(*@ pure *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
