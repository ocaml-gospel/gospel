type t

val y : t
(*@ ensures y = y *)

val modify_y : unit -> unit
(*@ modify_y ()
    modifies y *)
