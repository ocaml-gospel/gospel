(*@ open Ocamlstdlib *)

(* {gospel_expected|
   [125] File "open.mli", line 1, characters 3-21:
         1 | (*@ open Ocamlstdlib *)
                ^^^^^^^^^^^^^^^^^^
         Error: No module with name Ocamlstdlib.
   |gospel_expected} *)
