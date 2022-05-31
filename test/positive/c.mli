(*@ open Ocamlstdlib *)

open B

type t4 = t3
type t5 = int A.t2

(*@ function f5 (x: 'a A.t2) : A.t1 *)

(* {gospel_expected|
   [125] File "c.mli", line 1, characters 0-23:
         1 | (*@ open Ocamlstdlib *)
             ^^^^^^^^^^^^^^^^^^^^^^^
         Error: No module with name `Ocamlstdlib'.
   |gospel_expected} *)
