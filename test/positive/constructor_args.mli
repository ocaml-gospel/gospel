type t = C of int * int

val f : int -> int
(*@ m = f n
    (* Maybe this should not be accepted as it is not OCaml syntax *)
    requires C (n, n) = C n n
*)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
