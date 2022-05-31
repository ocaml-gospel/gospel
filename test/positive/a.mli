(*@ open Ocamlstdlib *)

type t1
type 'a t2

(*@ function f1 (x: t1) : int t2 *)

(*@ function f2 (x: int) : t1 *)

(*@ function f3 (x: int) : int t2 = f1 (f2 x) *)

(* EXPECTED
   [125] File "a.mli", line 1, characters 0-34:
         Error: No module with name `Ocamlstdlib'.
*)
