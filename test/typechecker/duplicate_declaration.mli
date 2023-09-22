type t

(*@ type t *)

(* {gospel_expected|
   [125] File "duplicate_declaration.mli", line 3, characters 9-10:
         3 | (*@ type t *)
                      ^
         Error: A declaration for t already exists in this context.
   |gospel_expected} *)
