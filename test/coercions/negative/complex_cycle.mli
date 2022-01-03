type t1
type t2
type t3

(*@ function c1 (x: t1) : t2 *)
(*@ coercion *)

(*@ function c2 (x: t2) : t3 *)
(*@ coercion *)

(*@ function c3 (x: t3) : t1 *)
(*@ coercion *)
