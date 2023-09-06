open Types_functions

type t3 = t1 * int t2

(*@ function f4 (x: int t2) (y: t1) : t3 = (y,x) *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
