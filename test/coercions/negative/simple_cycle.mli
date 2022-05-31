type t1
type t2

(*@ function c1 (x: t1) : t2 *)
(*@ coercion *)

(*@ function c2 (x: t2) : t1 *)
(*@ coercion *)

(* EXPECTED
   [125] File "simple_cycle.mli", line 7, characters 10-12:
         Error: This coercion introduces a cycle:
                  c1: t1 -> t2.
*)
