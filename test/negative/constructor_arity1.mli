type t =  C of int * int

val f : int -> t -> unit
(*@ f n t
    requires let x = (n, n) in C x = C x *)
