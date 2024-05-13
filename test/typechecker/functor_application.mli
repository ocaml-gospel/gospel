val tricky : Set.Make(Int).t -> bool
(* {gospel_expected|
   [125] gospel: internal error, uncaught exception:
                 Invalid_argument("Ppxlib.Longident.flatten")
                 
   |gospel_expected} *)
