type t1
type t2
type t3

(*@ function c1 (x: t1) : t2 *)
(*@ coercion *)

(*@ function c2 (x: t2) : t3 *)
(*@ coercion *)

(*@ function c3 (x: t3) : t1 *)
(*@ coercion *)

(* {gospel_expected|
   [125] File "complex_cycle.mli", line 11, characters 13-15:
         11 | (*@ function c3 (x: t3) : t1 *)
                           ^^
         Error: This coercion introduces a cycle:
                  c1: t1 -> t2
                  c2: t2 -> t3.
   |gospel_expected} *)
