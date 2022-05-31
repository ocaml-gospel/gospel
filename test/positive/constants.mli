val x : int ref
val incr_x : unit -> unit
(*@ incr_x ()
    modifies x
    ensures !x = old !x + 1 *)

type t

val y : t
val modify_y : unit -> unit
(*@ modify_y ()
    modifies y *)
(* EXPECTED
   [0] OK
*)
