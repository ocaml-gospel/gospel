(*@ open Ocamlstdlib *)

open A

type t3 = t1 * int t2

(*@ function f4 (x: int t2) (y: t1) : t3 = (y,x) *)

(* {gospel_expected|
   [125] File "b.mli", line 1, characters 0-23:
         1 | (*@ open Ocamlstdlib *)
             ^^^^^^^^^^^^^^^^^^^^^^^
         Error: No module with name `Ocamlstdlib'.
   |gospel_expected} *)
