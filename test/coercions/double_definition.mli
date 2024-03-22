type t1
type t2

(*@ function c1 (x: t1) : t2 *)
(*@ coercion *)

(*@ function c2 (x: t1) : t2 *)
(*@ coercion *)

(* {gospel_expected|
   [125] File "double_definition.mli", line 7, characters 13-15:
         7 | (*@ function c2 (x: t1) : t2 *)
                          ^^
         Error: A coercion between these types already exists:
                  c1: t1 -> t2.
   |gospel_expected} *)
