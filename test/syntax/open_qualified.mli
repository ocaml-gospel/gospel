open Open3

type t4 = t3
type t5 = int Types_functions.t2

(*@ function f5 (x: 'a Types_functions.t2) : Types_functions.t1 *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
