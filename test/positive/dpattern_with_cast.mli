val f : int option -> int
(*@ r = f x
    requires match x with
             | Some (y:int) -> y >= 0
             | None -> true *)

(* {gospel_expected|
   [0] OK
   |gospel_expected} *)
