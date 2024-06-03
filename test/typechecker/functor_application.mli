val tricky : Set.Make(Int).t -> bool
(* {gospel_expected|
   [125] File "functor_application.mli", line 1, characters 13-28:
         1 | val tricky : Set.Make(Int).t -> bool
                          ^^^^^^^^^^^^^^^
         Error: Functor application not supported: Set.Make(Int).t.
   |gospel_expected} *)
