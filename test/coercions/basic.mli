type t1
type t2

(*@ function c (x: t1) : t2 *)
(*@ coercion *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
