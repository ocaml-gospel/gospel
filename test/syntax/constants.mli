val x : int ref
val incr_x : unit -> unit
(*@ incr_x ()
    modifies x
    ensures !x = old !x + 1 *)

type t

val y : t
(*@ ensures y = y *)

val modify_y : unit -> unit
(*@ modify_y ()
    modifies y *)
