type t = C of int * int

val f : int -> t -> unit
(*@ f n t
    requires match t with
             | C _
             | _ -> true *)
