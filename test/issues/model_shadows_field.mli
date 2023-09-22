type 'a t = { contents : 'a list }
(*@ mutable model contents: 'a list *)

val f : 'a t -> unit
(*@ f xs
    modifies xs
    ensures xs = { contents = [] }
*)

(* {gospel_expected|
   [125] File "model_shadows_field.mli", line 7, characters 17-34:
         7 |     ensures xs = { contents = [] }
                              ^^^^^^^^^^^^^^^^^
         Error: The record field contents does not exist.
   |gospel_expected} *)
