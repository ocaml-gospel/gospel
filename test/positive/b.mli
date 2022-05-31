(*@ open Ocamlstdlib *)

open A

type t3 = t1 * int t2

(*@ function f4 (x: int t2) (y: t1) : t3 = (y,x) *)

(* EXPECTED
   [125] File "b.mli", line 1, characters 0-34:
         Error: No module with name `Ocamlstdlib'.
*)
