type t1
type t2

(*@ function c1 (x: t1) : t2 *)
(*@ coercion *)

(*@ function c2 (x: t2) : t1 *)
(*@ coercion *)

(* {gospel_expected|
   [125] File "simple_cycle.mli", line 7, characters 13-15:
         7 | (*@ function c2 (x: t2) : t1 *)
                          ^^
         Error: This coercion introduces a cycle:
                  c1: t1 -> t2.
   |gospel_expected} *)
